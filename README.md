# CoordinateCleaner 多国家匹配问题

### 背景

CoordinateCleaner包中的cc_coun函数在进行坐标验证时使用rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")作为默认地图。在一些特殊情况下，单个坐标点可能同时与多个国家发生匹配，这会导致cc_coun函数的iso-3验证失效。这种多重匹配现象通常出现在以下区域：

1. 边境地区
2. 争议领土
3. 边界复杂的小国家
4. 领海重叠的沿海区域

例如，位于(5.8°E, 50.8°N)的坐标点在rnaturalearth数据集中会同时匹配到荷兰（NLD）和比利时（BEL），这给数据清洗流程带来了验证困难。

### 新功能

为了解决这个问题，我们对cc_coun()函数进行了重写，新版本`cc_coun.multi.match()`增加了以下功能：

#### 1. 多重匹配处理选项

- `multi_match = "any"`：只要坐标点与声称的国家有任一匹配即视为有效
- `multi_match = "warn"`：接受匹配的同时发出警告提示
- `multi_match = "flag"`：将所有多重匹配标记为待处理问题

#### 2. 详细匹配信息

当启用`return_matches = TRUE`时，函数会返回完整的匹配详情：

- 问题点的索引位置
- 具体坐标信息
- 原始声称的国家代码
- 所有匹配到的国家代码

### 使用示例

```r
results <- cc_coun.multi.match(
 x = occurrence_data,
 multi_match = "warn",
 return_matches = TRUE
)

# 检查多重匹配结果
if(length(results$multi_matches) > 0) {
 print(results$multi_matches)
}
```

# CoordinateCleaner Multiple Country Matching Issue

### Background

The cc_coun function in CoordinateCleaner package uses rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") as its default map for coordinate validation. In certain cases, a single coordinate point may match multiple countries simultaneously, causing the iso-3 validation in cc_coun to fail. This multiple matching phenomenon typically occurs in the following areas:

1. Border regions
2. Disputed territories
3. Small countries with complex boundaries
4. Coastal areas with overlapping territorial waters

For instance, a coordinate point at (5.8°E, 50.8°N) matches both Netherlands (NLD) and Belgium (BEL) in the rnaturalearth dataset, creating validation challenges in the data cleaning process.

### New Features

To address this issue, we have rewritten the cc_coun() function. The new version `cc_coun.multi.match()` includes the following enhancements:

#### 1. Multiple Match Handling Options

- `multi_match = "any"`: Considers a point valid if it matches any of the claimed countries
- `multi_match = "warn"`: Accepts matches while issuing warning messages
- `multi_match = "flag"`: Marks all multiple matches as issues to be addressed

#### 2. Detailed Match Information

When `return_matches = TRUE` is enabled, the function returns comprehensive matching details:

- Index position of problematic points
- Specific coordinate information
- Originally claimed country code
- All matched country codes

### Usage Example

```r
results <- cc_coun.multi.match(
 x = occurrence_data,
 multi_match = "warn",
 return_matches = TRUE
)

# Check multiple match results
if(length(results$multi_matches) > 0) {
 print(results$multi_matches)
}
```
