# Cloud Seeding Data

Our lives depend on rainfall. Consequently, scientists have long
investigated whether humans can intervene and, as needed, help nature
produce more rainfall. In one study, researchers in southern Florida
explored whether injecting silver iodide into cumulus clouds would lead
to increased rainfall. On each of 52 days that were judged to be
suitable for cloud seeding, a target cloud was identified and a plane
flew through the target cloud in order to seed it. Randomization was
used to determine whether or not to load a seeding mechanism and seed
the target cloud with silver iodide on that day. Radar was used to
measure the volume of rainfall from the selected cloud during the next
24 hours. The results from Simpson, Olsen, and Eden, (1975) measure
rainfall in volume units of acre-feet, “height” of rain across one acre.

## Usage

``` r
CloudSeeding
```

## Format

### `CloudSeeding`

A data frame with 52 rows and 2 columns:

- treatment:

  Whether a cloud was seeded with silver iodide or not.

- rainfall:

  Volume of rainfall during the next 24 hours, in acre-feet.

## Source

[doi:10.2307/1268346](https://doi.org/10.2307/1268346)
