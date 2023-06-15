#' @name Chunk
#' @author 
#' Borja Latorre Garcés \url{http://eead.csic.es/home/staffinfo?Id=215}; Soil and Water, EEAD, CSIC \url{http://www.eead.csic.es}
#' Fergus Reig Gracia \url{http://fergusreig.es}; Environmental Hydrology, Climate and Human Activity Interactions, Geoenvironmental Processes, IPE, CSIC \url{http://www.ipe.csic.es/hidrologia-ambiental}
#' 
#' @details
#' \tabular{ll}{
#'   Version: \tab 1.0.0\cr
#'   License: \tab GPL version 3 or newer\cr
#' }
#'  
#' @description
#' From a netCDF file, generate two versions with the same data but with different chunk
#' configurations. In one case, favor the retrieval of temporal series of the main
#' variable in each pixel, and in the other, favor the retrieval of planes for each date.

#####################################################################
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# any later version.

# This program is distributed in the hope that it will be raster_3857,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program. If not, see <http://www.gnu.org/licenses/> <http://www.gnu.org/licenses/gpl.txt/>.
#####################################################################

#' @import ncdf4

library(ncdf4)

#' Create the new netCDF file with chunk dimensions that favor obtaining temporal series
#' for each pixel.
#' @param in_file Original netCDF file
#' @param out_file netCDF file with the same information as the original but with new chunk structure.
#' @param lon_by Number of pixels horizontally that will be read as a block during the read/write loop. -1 to read all at once.
#' @param lat_by Number of pixels vertically that will be read as a block during the read/write loop. -1 to read all at once.
#' @export
#' @examples
#' write_nc_chunk_t(in_file="ETo.nc", out_file="ETo-t.nc", lon_by=100, lat_by=100)
write_nc_chunk_t = function(in_file, out_file, lon_by = -1, lat_by = -1) {
    # Open the original netCDF file
    nc_in_file = nc_open(in_file)

    # Reads global attributes
    global_att = ncatt_get(nc_in_file, 0)

    # Read attributes of dimensions and variable
    lon_longname_att = ncatt_get(nc_in_file, "lon", "long_name")
    lon_longname = if(lon_longname_att$hasatt) lon_longname_att$value else "longitude"
    lon_units_att = ncatt_get(nc_in_file, "lon", "units")
    lon_units = if(lon_units_att$hasatt) lon_units_att$value else "m"

    lat_longname_att = ncatt_get(nc_in_file, "lat", "long_name")
    lat_longname = if(lat_longname_att$hasatt) lat_longname_att$value else "latitude"
    lat_units_att = ncatt_get(nc_in_file, "lat", "units")
    lat_units = if(lat_units_att$hasatt) lat_units_att$value else "m"

    time_longname_att = ncatt_get(nc_in_file, "time", "long_name")
    time_longname = if(time_longname_att$hasatt) time_longname_att$value else "time"
    time_units_att = ncatt_get(nc_in_file, "time", "units")
    time_units = if(time_units_att$hasatt) time_units_att$value else "days since 1970-01-01"
    time_calendar_att = ncatt_get(nc_in_file, "time", "calendar")
    time_calendar = if(time_calendar_att$hasatt) time_calendar_att$value else "gregorian"

    var_longname_att = ncatt_get(nc_in_file, "ETo", "long_name")
    var_longname = if(var_longname_att$hasatt) var_longname_att$value else "?"
    var_units_att = ncatt_get(nc_in_file, "ETo", "units")
    var_units = if(var_units_att$hasatt) var_units_att$value else "?"

    # Reads the dimensions of the original file
    lon_data = ncvar_get(nc_in_file, "lon")
    lat_data = ncvar_get(nc_in_file, "lat")
    time_data = ncvar_get(nc_in_file, "time")

    # Sizes of dimensions
    lon_num = length(lon_data)
    lat_num = length(lat_data)
    time_num = length(time_data)

    # Checks the size of the read/write batches used for processing large files
    lon_by = if (lon_by < 1 || lon_by >= lon_num) lon_num else lon_by
    lat_by = if (lat_by < 1 || lat_by >= lat_num) lat_num else lat_by

    # Define the dimensions for the final file
    lon = ncdim_def("lon", lon_units, lon_data, longname=lon_longname)
    lat = ncdim_def("lat", lat_units, lat_data, longname=lat_longname)
    time = ncdim_def("time", time_units, time_data, longname=time_longname,
                     calendar=time_calendar)
    var = ncvar_def("ETo", var_units, list(lon, lat, time), chunksizes=c(1,1,time_num),
                     longname=var_longname, compression=9)

    # Final file creation
    nc_out_file = nc_create(out_file, list(var), force_v4 = TRUE)

    if (lon_by == lon_num && lat_by == lat_num) {
        # Read/write data at once
        ETo_data = ncvar_get(nc_in_file, var)
        ncvar_put(nc_out_file, var, ETo_data)
    } else {
        # Read/write data in batches
        for (x in seq(1, lon_num, by=lon_by)) {
            x_rest = lon_num - x + 1
            x_count = if (x_rest >= lon_by) lon_by else x_rest
            for (y in seq(1, lat_num, by=lat_by)) {
                y_rest = lat_num - y + 1
                y_count = if (y_rest >= lat_by) lat_by else y_rest
                ETo_data = ncvar_get(nc_in_file, var, start=c(x,y,1), count=c(x_count,y_count,time_num))
                ncvar_put(nc_out_file, var, ETo_data, start=c(x,y,1), count=c(x_count,y_count,time_num))
            }
        }
    }

    # Writes the global attributes to the final file
    for (name in names(global_att)) {
        ncatt_put(nc_out_file, 0, name, global_att[[name]])
    }

    nc_close(nc_out_file)
    nc_close(nc_in_file)
}


#' Create the new netCDF file with favorable chunk dimensions to obtain plans for each date.
#' @param in_file Original netCDF file
#' @param out_file netCDF file with the same information as the original but with new chunk structure.
#' @param time_by Number of dates that will be read as a block during the read/write loop. -1 to read all at once.
#' @export
#' @examples
#' write_nc_chunk_xy(in_file="ETo.nc", out_file="ETo-xy.nc", time_by=100)
write_nc_chunk_xy = function(in_file, out_file, time_by = -1) {
    # Open the original netCDF file
    nc_in_file = nc_open(in_file)

    # Reads global attributes
    global_att = ncatt_get(nc_in_file, 0)

    # Read attributes of dimensions and variable
    lon_longname_att = ncatt_get(nc_in_file, "lon", "long_name")
    lon_longname = if(lon_longname_att$hasatt) lon_longname_att$value else "longitude"
    lon_units_att = ncatt_get(nc_in_file, "lon", "units")
    lon_units = if(lon_units_att$hasatt) lon_units_att$value else "m"

    lat_longname_att = ncatt_get(nc_in_file, "lat", "long_name")
    lat_longname = if(lat_longname_att$hasatt) lat_longname_att$value else "latitude"
    lat_units_att = ncatt_get(nc_in_file, "lat", "units")
    lat_units = if(lat_units_att$hasatt) lat_units_att$value else "m"

    time_longname_att = ncatt_get(nc_in_file, "time", "long_name")
    time_longname = if(time_longname_att$hasatt) time_longname_att$value else "time"
    time_units_att = ncatt_get(nc_in_file, "time", "units")
    time_units = if(time_units_att$hasatt) time_units_att$value else "days since 1970-01-01"
    time_calendar_att = ncatt_get(nc_in_file, "time", "calendar")
    time_calendar = if(time_calendar_att$hasatt) time_calendar_att$value else "gregorian"

    var_longname_att = ncatt_get(nc_in_file, "ETo", "long_name")
    var_longname = if(var_longname_att$hasatt) var_longname_att$value else "?"
    var_units_att = ncatt_get(nc_in_file, "ETo", "units")
    var_units = if(var_units_att$hasatt) var_units_att$value else "?"

    # Reads the dimensions of the original file
    lon_data = ncvar_get(nc_in_file, "lon")
    lat_data = ncvar_get(nc_in_file, "lat")
    time_data = ncvar_get(nc_in_file, "time")

    # Sizes of dimensions
    lon_num = length(lon_data)
    lat_num = length(lat_data)
    time_num = length(time_data)

    # Checks the size of the read/write batches used for processing large files
    time_by = if (time_by < 1 || time_by >= time_num) time_num else time_by
 
    # Define the dimensions for the final file
    lon = ncdim_def("lon", lon_units, lon_data, longname=lon_longname)
    lat = ncdim_def("lat", lat_units, lat_data, longname=lat_longname)
    time = ncdim_def("time", time_units, time_data, longname=time_longname,
                     calendar=time_calendar)
    var = ncvar_def("ETo", var_units, list(lon, lat, time), chunksizes=c(lon_num,lat_num,1),
                     longname=var_longname, compression=9)

    # Final file creation
    nc_out_file = nc_create(out_file, list(var), force_v4 = TRUE)

    if (time_by == time_num) {
        # Read/write data at once
        ETo_data = ncvar_get(nc_in_file, var)
        ncvar_put(nc_out_file, var, ETo_data)
    } else {
        # Read/write data in batches
        for (t in seq(1, time_num, by=time_by)) {
            t_rest = time_num - t + 1
            t_count = if (t_rest >= time_by) time_by else t_rest
            ETo_data = ncvar_get(nc_in_file, var, start=c(1,1,t), count=c(lon_num,lat_num,t_count))
            ncvar_put(nc_out_file, var, ETo_data, start=c(1,1,t), count=c(lon_num,lat_num,t_count))
        }
    }

    # Writes the global attributes to the final file
    for (name in names(global_att)) {
        ncatt_put(nc_out_file, 0, name, global_att[[name]])
    }

    nc_close(nc_out_file)
    nc_close(nc_in_file)
}


create_nc_name = function(file_name, sufix="-t") {
    pos = unlist(gregexpr(".nc", file_name))
    ext_pos = pos[length(pos)]
    return(paste(substr(file_name,1,ext_pos-1), sufix, substr(file_name,ext_pos,nchar(file_name)), sep=""))
}


# nc_route <- "../viewer/nc"
# ncFile <- "ETo.nc"
# file <- file.path(nc_route, ncFile)
# t_file = file.path(nc_route, create_nc_name(ncFile))
# lon_by = 100
# lat_by = 100
# write_nc_chunk_t(in_file=file, out_file=t_file, lon_by=lon_by, lat_by=lat_by)


nc_route <- "../viewer/nc"
ncFile <- "ETo.nc"
file <- file.path(nc_route, ncFile)
xy_file = file.path(nc_route, create_nc_name(ncFile, sufix="-xy"))
time_by = 100
write_nc_chunk_xy(in_file=file, out_file=xy_file, time_by=time_by)
