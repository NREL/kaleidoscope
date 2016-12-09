# Data formats for kaleidoscope inputs

## Generation Data

`generation_Period_scenario.csv` :

| Generator Name | 2006-11-26 19:20:00 | 2006-11-26 19:25:00 | ... |
|----------------|---------------------|---------------------|-----|
| Gen_1          | 10                  | 12.5                | ... |


## Interchange Data

`net_interchange.Rdata`

| time       | scenario  | Source2Sink | value         |
|------------|-----------|-------------|---------------|
| 2006-01-01 | c_RT_loVG | FRCC - SERC | -1.000024e-02 |
| 2006-01-01 | c_RT_loVG | HQ - IESO   | 2.871305e+03  |
| 2006-01-01 | c_RT_loVG | HQ - ISO-NE | 7.667651e+02  |
| 2006-01-01 | c_RT_loVG | HQ - NBSO   | 1.392201e+03  |
| 2006-01-01 | c_RT_loVG | HQ - NYISO  | 1.988000e+03  |
| 2006-01-01 | c_RT_loVG | IESO - HQ   | -2.871305e+03 |

## Dispatch Stack Data

`dispatch_stack_data.Rdata`

| Type | time       | zone   | value    | scenario  | partition |
|------|------------|--------|----------|-----------|-----------|
| Coal | 2006-01-01 | FRCC   | 7340.70  | c_RT_loVG | 1         |
| Coal | 2006-01-01 | IESO   | 2402.00  | c_RT_loVG | 1         |
| Coal | 2006-01-01 | ISO-NE | 1895.90  | c_RT_loVG | 1         |
| Coal | 2006-01-01 | MISO   | 57613.51 | c_RT_loVG | 1         |
| Coal | 2006-01-01 | NBSO   | 2990.00  | c_RT_loVG | 1         |
| Coal | 2006-01-01 | NYISO  | 1701.45  | c_RT_loVG | 1         |


## Generator Data

`gen.csv`

| Node_Name          | Generator_Name              | Node_Region | Type  | lat    | lon     |
|--------------------|-----------------------------|-------------|-------|--------|---------|
| 100001_CHESTER_345 | N30P_Wind_100001            | ISO-NE      | Wind  | 45.395 | -68.523 |
| 100001_CHESTER_345 | loVG_Wind_100001            | ISO-NE      | Wind  | 45.395 | -68.523 |
| 100001_CHESTER_345 | R30P_Wind_100001            | ISO-NE      | Wind  | 45.395 | -68.523 |
| 100001_CHESTER_345 | Beaver Wood Joint Venture_1 | ISO-NE      | Other | 45.395 | -68.523 |
| 100001_CHESTER_345 | SRPS_Wind_100001            | ISO-NE      | Wind  | 45.395 | -68.523 |
| 100003_MAXCYS_345  | R30P_PV_100003              | ISO-NE      | PV    | 44.164 | -69.415 |


## Region Centroids

`centroids.csv`

| ISO      | lon       | lat      |
|----------|-----------|----------|
| FRCC     | -81.58157 | 28.40805 |
| HQ       | -72.38762 | 53.13232 |
| IESO     | -84.25279 | 50.46110 |
| ISO-NE   | -70.58611 | 44.49746 |
| Manitoba | -98.23007 | 55.80747 |
| MISO     | -95.50000 | 45.50000 |

## Generator Colors

```R
colors = data.table(type=c("Hydro", "Nuclear", "Coal", "Gas CC", "Wind", "CT/Gas boiler", "Other", "Pumped Storage", "PV", "CHP-QF", "Geothermal", "Storage", "Biomass", "CSP", "Steam", "DR", "RPV"),
                       color=c("#add8e6","#b22222","#333333","#6e8b3d","#4f94cd", "#ffb6c1","#8968cd", "#FFFFFF", "#ffc125", "gray20", "khaki1", "gray45", "mediumpurple2", "darkorange2", "orchid4", "gray60", "goldenrod2"))
```

## ISOs

```R
iso = c("Saskatchewan", "PJM", "SPP", "NBSO", "SERC", "IESO", "ISO-NE", "Manitoba", "NYISO", "MISO", "HQ", "FRCC")
```

