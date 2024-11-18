#' Identify Coordinates Outside their Reported Country
#'
#' Removes or flags mismatches between geographic coordinates and additional
#' country information (usually this information is reliably reported with
#' specimens). Such a mismatch can occur for example, if latitude and longitude
#' are switched.
#'
#'
#' @param iso3 a character string. The column with the country assignment of
#'   each record in three letter ISO code. Default = \dQuote{countrycode}.
#' @param ref SpatVector (geometry: polygons). Providing the geographic
#'   gazetteer. Can be any SpatVector (geometry: polygons), but the structure
#'   must be identical to \code{rnaturalearth::ne_countries(scale = "medium",
#'   returnclass = "sf")}.
#'   Default = \code{rnaturalearth::ne_countries(scale = "medium", returnclass =
#'   "sf")}
#' @param ref_col the column name in the reference dataset, containing the
#'   relevant ISO codes for matching. Default is to "iso_a3_eh" which refers to
#'   the ISO-3 codes in the reference dataset. See notes.
#' @param buffer numeric. Units are in meters. If provided, a buffer is
#'   created around each country polygon.
#'
#' @inheritParams cc_cen
#'
#' @inherit cc_cap return
#'
#' @note The ref_col argument allows to adapt the function to the structure of
#'   alternative reference datasets. For instance, for
#'   \code{rnaturalearth::ne_countries(scale = "small")}, the default will fail,
#'   but ref_col = "iso_a3" will work.
#'
#' @note With the default reference, records are flagged if they fall outside
#'   the terrestrial territory of countries, hence records in territorial waters
#'   might be flagged. See \url{https://ropensci.github.io/CoordinateCleaner/}
#'   for more details and tutorials.
#'
#' @keywords Coordinate cleaning
#' @family Coordinates
#'
#' @examples
#'
#' \dontrun{
#' x <- data.frame(species = letters[1:10],
#'                 decimalLongitude = runif(100, -20, 30),
#'                 decimalLatitude = runif(100, 35,60),
#'                 countrycode = "RUS")
#'
#' cc_coun(x, value = "flagged")#non-terrestrial records are flagged as wrong.
#' }
#'
#' @export
#' @importFrom terra vect geomtype extract
#' @importFrom stats na.omit
#'
#' @param multi_match character. How to handle points matching multiple countries: "any", "warn", or "flag"
#' @param return_matches logical. If TRUE returns detailed matching information
#'
#' @return Depending on value and return_matches parameters:
#' \itemize{
#'   \item If value = "clean": returns cleaned dataset
#'   \item If value = "flagged": returns logical vector
#'   \item If return_matches = TRUE: returns list with cleaning results and match details
#' }
#'

cc_coun.multi.match <- function(x,
                    lon = "decimalLongitude",    # 经度列名
                    lat = "decimalLatitude",     # 纬度列名
                    iso3 = "countrycode",        # 国家代码列名
                    value = "clean",             # 输出类型：clean(清理后的数据)或flagged(逻辑向量)
                    ref = NULL,                  # 参考地图数据
                    ref_col = "iso_a3",          # 参考地图中的国家代码列名
                    verbose = TRUE,              # 是否显示处理信息
                    buffer = NULL,               # 缓冲区大小（米）
                    multi_match = "warn",      # 多重匹配处理方式：any(接受任意匹配)/warn(警告)/flag(标记为问题)
                    return_matches = FALSE) {

  # 验证输入参数
  # check function arguments for validity
match.arg(value, choices = c("clean", "flagged"))
match.arg(multi_match, choices = c("any", "warn", "flag"))

  # 检查必需的国家代码列是否存在
if (!iso3 %in% names(x)) {
  stop("iso3 argument missing, please specify")
}

if (verbose) {
  message("Testing country identity")
}

  # set reference and check for dependency
if (is.null(ref)) {
  # 设置参考数据：如果未提供，使用rnaturalearth包的数据
  if (!requireNamespace("rnaturalearth", quietly = TRUE)) {
    stop("Install the 'rnaturalearth' package or provide a custom reference",
         call. = FALSE
         )
  }
  ref <- terra::vect(rnaturalearth::ne_countries(scale = "medium",
                                                 returnclass = "sf"))
} else {
    # 处理自定义参考地图
    # 转换空间数据格式
    # Enable sf formatted custom references
  if (any(is(ref) == "Spatial")  | inherits(ref, "sf")) {
    ref <- terra::vect(ref)
  }
    # 验证空间数据类型
    # Check if object is a SpatVector
  if (!(inherits(ref, "SpatVector") &
        terra::geomtype(ref) == "polygons")) {
    stop("ref must be a SpatVector with geomtype 'polygons'")
  }
    # 重投影参考地图
    # Check projection of custom reference and reproject if necessary
  ref <- reproj(ref)
}

# 准备坐标数据
  dat <- terra::vect(x[, c(lon, lat)],
                     geom = c(lon, lat),
                     crs = ref)

  # 处理缓冲区
  if (is.numeric(buffer)) {
    # 避免0缓冲区的问题
    buffer <- ifelse(buffer == 0, 0.00000000001, buffer)
    # 创建缓冲区
    ref_buff <- terra::buffer(ref, buffer)
    # 处理缓冲区数据
    ref <- terra::vect(stats::na.omit(terra::geom(ref_buff)),
                       type = "polygon", crs = ref)
    terra::values(ref) <- terra::values(ref_buff)
  }

  # 提取坐标对应的国家信息
  country <- terra::extract(ref, dat)

  # 获取数据中声称的国家代码
  count_dat <- as.character(unlist(x[, iso3]))

  # 初始化输出向量和多重匹配点记录
  out <- logical(length(dat))
  multi_match_points <- list()  # 使用列表存储多重匹配信息

  # 逐点处理匹配结果
  for(i in seq_along(dat)) {
    # 获取当前点匹配的所有国家
    matches <- country[country[,1] == i, ref_col]

    if(length(matches) > 1) {

      # 存储多重匹配信息
      multi_match_points[[length(multi_match_points) + 1]] <- list(
        index = i,
        coordinates = c(x[i, lon], x[i, lat]),
        claimed_country = count_dat[i],
        matched_countries = matches
      )

      # 根据多重匹配处理模式进行处理
      switch(multi_match,
             "any" = {
               # 接受任意匹配
               out[i] <- count_dat[i] %in% matches
             },
             "warn" = {
               # 接受匹配但发出警告
               out[i] <- count_dat[i] %in% matches
             },
             "flag" = {
               # 将多重匹配标记为问题
               out[i] <- FALSE
             })
    } else if(length(matches) == 1) {
      # 单一匹配情况
      out[i] <- count_dat[i] == matches
    } else {
      # 没有匹配情况
      out[i] <- FALSE
    }
  }

  # 处理多重匹配的警告信息
  if (length(multi_match_points) > 0 && multi_match == "warn") {
    warning(sprintf("Found %d points matching multiple countries. Using 'any' match criterion.",
                   length(multi_match_points)))
  }

  # return output
  if (verbose) {
    if (value == "clean") {
      message(sprintf("Removed %s records.", sum(!out)))
    } else {
      message(sprintf("Flagged %s records.", sum(!out)))
    }
  }


# 根据参数返回不同结果
result <- if(return_matches) {
  list(
    cleaning_result = switch(value,
                           clean = x[out, ],
                           flagged = out),
    multi_matches = multi_match_points
  )
} else {
  switch(value,
         clean = x[out, ],
         flagged = out)
}

return(result)

}
