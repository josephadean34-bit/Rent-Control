# Rent Control in Washington, DC: Building a Property-Level Dataset and Estimating the Rent Differential

This project supported a research project focused on examining the impact of rent control of the Washington, DC

No public dataset flags which properties in Washington, DC are subject to rent
stabilization. The Rental Housing Act of 1985 defines coverage through a
combination of construction date, ownership structure, subsidy status, and tax
treatment — none of which appears as a single field in any one source.

This project reconstructs that eligibility test from nine public records datasets to
build a parcel-level rent control flag for the entire District, merges the result
with proprietary listing-level rent data from Altos Research, and estimates the
rent differential using a two-way fixed effects hedonic model.

**Rent-controlled apartment properties list at roughly 10–13% below comparable
uncontrolled properties in the same census tract and year.**

All code is in `RCode.R`. Source dataset links are in `requirements.md`.

---

## Data sources

| Source | Role |
|---|---|
| Integrated Tax System Public Extract (ITSPE) | Base parcel roll: use code, owner name, owner address, homestead exemption, tax type |
| CAMA Residential / Commercial / Condominium | Building characteristics: unit counts, year built, stories, gross building area |
| Address Residential Units (ARU) | Unit-level address file, used to count units per building |
| Condo Regime, Condo Relate, Condo Approval Lots, Common Ownership Lots | Condominium structure reference tables |
| National Housing Preservation Database | Subsidized property identification |
| Altos Research / CoStar | Listing-level asking rents and building amenities (proprietary) |

Parcels join on **SSL** (Square-Suffix-Lot), DC's parcel identifier. Rent data joins
on a standardized address string.

---

## Part 1: Building the rent control dataset

The statute covers rental housing built before 1976, with four exemptions:
buildings owned by natural persons holding fewer than five rental units in the
District, federally or locally subsidized properties, publicly owned properties, and
properties exempt from real property tax. Every one of those conditions has to be
constructed.

### Property classification and building characteristics

ITSPE `USECODE` values collapse into eight property type groupings — apartments,
condo, conversion, cooperative, flat, investment condo, single-family, other — which
drive nearly every downstream rule. Groupings 1, 3, and 5 (apartments, conversions,
flats) form the multifamily universe the analysis is restricted to. Cooperatives are
excluded from the rental definition entirely, since shareholders are owners rather
than tenants.

The three CAMA files cover disjoint property types, so they are stacked and joined
to the tax roll on SSL. Mixed-use parcels appear in more than one file; the
deduplication keeps the SSL where unique and otherwise keeps the apartment record.
`AYB` (actual year built) comes from CAMA and drives the pre-1976 coverage test.

### Address standardization

Addresses are the join key to both ARU and Altos, and none of the three sources
formats them the same way. The key observation is that every DC address terminates
in one of four quadrant codes, which makes a full address parser unnecessary:

```r
mutate(ADDY = sub("^(.*?(NW|NE|SW|SE)).*$", "\\1", PREMISEADD))
```

Everything after the quadrant — city, state, ZIP — is discarded. A mapping table
then reconciles street type conventions. ARU spells types out, so abbreviations are
expanded (`ST` → `STREET`); Altos abbreviates, so the merge in Part 2 runs the same
normalization in reverse.

### Constructing the unit count

Unit counts matter twice: as a building characteristic in the model, and as the
input to the small-landlord exemption that partly defines treatment. No single
source has complete coverage.

ARU is aggregated to a per-address count, with non-condo addresses collapsed to one
row and condo records retained individually. `UNITS_MASTER` then resolves CAMA and
ARU by property type, preferring CAMA's `NUM_UNITS` and falling back to the ARU
count. Condos, investment condos, and single-family properties are set to 1 by
definition.

Where both sources are missing, counts are imputed by OLS on building size, fit
separately by type:

```r
model_apts  <- lm(UNITS_MASTER ~ LIVING_GBA, data = train_data)
model_flats <- lm(UNITS_MASTER ~ STORIES + LIVING_GBA, data = train_data)
```

Flats get a stories term because they are typically two- to four-unit structures
where floor count is informative; apartment buildings vary too much in floorplate
for stories alone to help. Records still missing after imputation take the
property-type mean.

### Identifying rental properties

Whether a property is a rental is inferred from two signals: the homestead
deduction, which is only available on a primary residence, and whether the owner's
mailing address matches the property address.

```r
MATCH1 = if_else(Owners_Street_Number == Property_Street_Number, 1, 0, missing = 0),
MATCH2 = if_else(OWNER_ADDRESS == PROPERTY_ADDRESS, 1, 0, missing = 0),
HCODE15 = if_else(HSTDCODE == 1 | HSTDCODE == 5, 1, 0, missing = 0),
OWNER_OCCUPIED = if_else(HCODE15 == 1 | MATCH == 1, 1, 0, missing = 0)
```

Matching on street number rather than the full string is deliberate — it tolerates
formatting differences between the owner and property address fields at the cost of
some false positives on the same block. `RENTAL` then applies type-specific logic:
apartments always, cooperatives never, and the rest conditional on owner occupancy.

### The four exemptions

**Construction era.** `AYB` buckets into pre-1976, 1976–77, unregulated (1978–2007),
and IZ-era (2007+). The `AYB != 0` guard matters: zero is CAMA's missing-value
sentinel, and without it every unknown-vintage parcel would read as built in year
zero and qualify as pre-1976.

**Small landlord.** The statute exempts natural persons owning fewer than five
rental units District-wide, which requires both a portfolio count and a test for
whether the owner is a person or an entity. Portfolio size aggregates rental units
across the owner's mailing address, a proxy for common ownership across parcels.
Personhood is tested by searching the owner name against a regex of roughly ninety
corporate, governmental, and institutional identifiers — `LLC`, `TRUST`,
`PARTNERSHIP`, `CHURCH`, `UNIVERSITY`, `EMBASSY`, `AUTHORITY`, and so on. A name
containing none of them is treated as a natural person; a missing owner name
defaults to *not* a natural person, the conservative direction.

**Subsidized** properties come from the National Housing Preservation Database by
address join. **Publicly owned** properties are identified by owner name against the
District, DCHA, and the federal government. **Non-taxable** properties come from the
tax type field (`E[0-9]`, `US`, `DC`).

### The flag

```r
RENT_CONTROLLED = if_else(
  (REG_ERA == "Unknown" | REG_ERA == "Built before 1976") &
    NON_TAXABLE == 0 & U5RULE1 == 0 & PUBLICLY_OWNED == 0 & SUBSIDIZED == 0,
  1, 0, missing = NA)
```

Unknown-vintage parcels are grouped with pre-1976 rather than excluded. DC's
unknown-vintage stock skews old, so this is a defensible default, but it is a
researcher decision rather than a statutory one — see Limitations.

Multifamily buildings sharing a single address are dropped before the merge, since
they cannot be distinguished from one another when joining on address.

---

## Part 2: Merging with Altos

The property dataset is parcel-level and static; Altos is listing-level and
time-stamped. The merge joins them on address and attaches a census tract for fixed
effects.

### Geocoding to census tracts

The multifamily subset joins to a geocode file on SSL, converts to spatial points,
and intersects with 2020 tract boundaries from `tigris`:

```r
tracts_DC <- tracts(state = "DC", year = 2020, cb = TRUE)
points_sf <- st_as_sf(RC1, coords = c("X","Y"), crs = 4326)
points_sf <- st_transform(points_sf, st_crs(tracts_DC))
RC1Polygons <- st_join(tracts_DC, points_sf, join = st_contains)
```

Points are created in WGS84 and transformed to the tract CRS before joining.
Skipping that transform is the standard failure mode here — it produces an empty
join rather than an error.

### Address matching

Altos arrives in two files: listing-level records keyed on `ADDR_NORM`, and
building amenities keyed on `street_address`. Both run through a standardization
function that reverses the earlier normalization, contracting full street names to
abbreviations to match Altos convention. Quadrant terms are replaced before street
types, since `NORTHWEST`-style spellings would otherwise interfere with the
terminator extraction that follows.

```r
ADDY = str_extract(standardized_address, ".*\\b(NW|NE|SW|SE)\\b")
```

An inner join on `ADDY` is the binding constraint on sample size — only addresses
appearing in both the tax roll and Altos survive. Amenities are collapsed to one row
per address before joining, since the amenity file carries multiple listings per
building.

### Sample restrictions

Studios through three-bedrooms; multifamily only; a hard price band of $500 to
$10,000; then a Tukey IQR rule applied **within** property type and bedroom count
rather than across the pooled sample, so a large three-bedroom is not flagged as an
outlier against a pool dominated by studios.

Factor levels are set so apartments and one-bedrooms are the reference categories —
every reported coefficient reads against a one-bedroom apartment.

---

## Part 3: The regression model

```r
reg <- RC3Polygons %>%
  filter(SUBSIDIZED == 0 & PUBLICLY_OWNED == 0 & NON_TAXABLE == 0)

model <- felm(
  log(last_listing_price) ~ RENT_CONTROLLED + BuildingUnits + SQFT + beds +
                            PROPERTY_TYPE_GROUPING + pool + gym
                          | GEOID + YEAR,
  data = reg)
```

A hedonic price regression with two-way fixed effects, estimated with `felm` from
the `lfe` package, which absorbs high-dimensional factors rather than expanding them
into dummies.

**Log rent** means the `RENT_CONTROLLED` coefficient reads directly as an
approximate percentage differential, and it handles the right skew typical of rent
distributions.

**Controls** are the hedonic characteristics that determine what a unit is worth
independent of its regulatory status: building size, unit square footage, bedroom
count, property type, and two amenities.

**Fixed effects.** The tract effect absorbs every time-invariant neighborhood
characteristic — school quality, transit access, amenity density, baseline
desirability — so identification comes from comparing controlled and uncontrolled
buildings *within the same tract*. The year effect absorbs District-wide rent
inflation and market cycles.

**The filter** drops subsidized, publicly owned, and non-taxable properties. These
are already excluded from `RENT_CONTROLLED` by construction, so leaving them in
would load them into the comparison group and bias the estimate — their rents are
set administratively, not by the market.

---

## Limitations

### Identification is cross-sectional, not a natural experiment

Despite the two-way fixed effects, this is not a difference-in-differences design.
Rent control status in DC does not change over the sample period — coverage is fixed
by a 1976 construction cutoff set decades ago. The tract and year effects absorb
location and time, but the estimate is identified off cross-sectional variation
between controlled and uncontrolled buildings within a tract, under a
selection-on-observables assumption. The coefficient is an association conditional
on the included hedonics, not a causal policy effect.

### Building age is confounded with treatment

This is the most consequential issue. `RENT_CONTROLLED` is defined primarily by
pre-1976 construction, so the treated group is by construction the older building
stock. Any rent differential attributable to vintage — dated finishes and systems,
smaller unit layouts, absence of modern amenities — loads onto the rent control
coefficient. `AgeOfBuilding` is computed in the pipeline but does not enter the
model.

Adding it would not fully resolve the problem, since age and treatment are nearly
collinear by definition. The 10–13% figure should be read as an upper bound on the
regulatory effect. A regression discontinuity around the 1976 cutoff is the natural
way to separate the two, and is the obvious next step for this project.

### Standard errors are not clustered

The `felm` call supplies covariates and fixed effects but no cluster specification.
With multiple listings per building and per tract, residuals are almost certainly
correlated within those groups and the reported standard errors will be too small.
Clustering at the building or tract level should be added before inference is
reported.

### Unknown-vintage properties are assigned to treatment

Properties land in `"Unknown"` when `AYB` is zero or missing, and they are grouped
with pre-1976 in the treatment definition. If missing vintage correlates with
characteristics that also affect rent, this introduces measurement error in the
treatment variable. A robustness check restricting to `"Built before 1976"` only
would quantify the sensitivity.

### Imputed unit counts propagate into treatment

`UNITS_MASTER` is OLS-imputed where CAMA and ARU both lack coverage. That variable
feeds the small-landlord exemption, one of the four conditions defining
`RENT_CONTROLLED`, and also enters the regression directly as `BuildingUnits`. The
exposure is limited — the exemption threshold is low and imputation mostly affects
larger buildings — but measurement error touches both the treatment and a covariate.

### Address matching is lossy and unquantified

The merge depends on string matching across three differently formatted sources.
Multifamily buildings sharing an address are dropped, and any listing whose
standardized address fails to match the tax roll is lost to the inner join. Match
rates are not reported at any stage. If matching success correlates with building
age — plausible, since older buildings have messier address records — the analysis
sample may not be representative of the universe.

### Asking rents, not contract rents

Altos records listing prices. Rent control binds on rent increases for sitting
tenants, so its effect is most visible in long-tenured units that never appear as
listings. Units reaching the market are disproportionately those with turnover,
where landlords can reset rents under vacancy provisions. The listing-based estimate
likely understates the differential in the occupied stock.
