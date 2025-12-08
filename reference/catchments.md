# Example catchment boundary polygons.

Two example catchment boundaries as a SpatialPolygonsDataFrame. The
catchments are Creswick Creek (ID 407214, Vic., Australia, see
http://www.bom.gov.au/water/hrs/#id=407214) and Bet Bet Creek (ID
407220, Vic., Australia, see
http://www.bom.gov.au/water/hrs/#id=407220). The catchments can be used
to extract catchment average climate data usng `extractCatchmentData`

## Usage

``` r
catchments
```

## Format

An object of class `SpatialPolygonsDataFrame` with 2 rows and 1 columns.

## See also

[`extractCatchmentData`](https://peterson-tim-j.github.io/AWAPer/reference/extractCatchmentData.md)
for extracting catchment average climate data.

## Examples

``` r
# Load example catchment boundaries.
data("catchments")
```
