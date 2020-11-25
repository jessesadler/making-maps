# ggmap: get map

[GoogleMaps Styling Wizard](https://mapstyle.withgoogle.com)

* [Style elements for google maps](https://developers.google.com/maps/documentation/static-maps/styling)
	* No country and city labels: element:labels|visibility:off
	* No roads: feature:road|visibility:off
	* No borders: feature:administrative|visibility:off
	* Colors: Hex colors with 0x in place of #
		* Geometry: element:geometry|color:0x1d2c4d
		* Landscape: feature:landscape.natural|color:0x023e58
		* Water: feature:water|color:0x0e1626

# Black and white map with borders for countries
bw_map_borders <- get_googlemap(center = c(3.5, 46.5), zoom = 5, color = "bw", style = "feature:road|visibility:off&style=element:labels|visibility:off")

# Black and white map without borders
bw_map <- get_googlemap(center = c(3.5, 46.5), zoom = 5, color = "bw", style = "feature:road|visibility:off&style=element:labels|visibility:off&style=feature:administrative|visibility:off")

# Aubergine 
dark_map <- get_googlemap(center = c(3.5, 46.5), zoom = 5, style = "feature:road|visibility:off&style=element:labels|visibility:off&style=feature:administrative|visibility:off&style=element:geometry|color:0x023e58&style=feature:landscape.natural|color:0x023e58&style=feature:water|color:0x0e1626")


style = "feature:road|visibility:off&style=element:labels|visibility:off&style=feature:administrative|visibility:off&style=element:geometry|color:0x1d2c4d&style=feature:landscape.natural|color:0x023e58&style=feature:water|color:0x0e1626"


style=element:geometry%7Ccolor:#1d2c4d

style=element:labels%7Cvisibility:off
style=element:labels.text.fill%7Ccolor:0x8ec3b9
style=element:labels.text.stroke%7Ccolor:0x1a3646

style=feature:administrative%7Celement:geometry%7Cvisibility:off
style=feature:administrative.country%7Celement:geometry.stroke%7Ccolor:0x4b6878
style=feature:administrative.land_parcel%7Celement:labels.text.fill%7Ccolor:0x64779e
style=feature:administrative.neighborhood%7Cvisibility:off
style=feature:administrative.province%7Celement:geometry.stroke%7Ccolor:0x4b6878

style=feature:landscape.man_made%7Celement:geometry.stroke%7Ccolor:0x334e87
style=feature:landscape.natural%7Celement:geometry%7Ccolor:0x023e58

style=feature:poi%7Cvisibility:off
style=feature:poi%7Celement:geometry%7Ccolor:0x283d6a
style=feature:poi%7Celement:labels.text.fill%7Ccolor:0x6f9ba5
style=feature:poi%7Celement:labels.text.stroke%7Ccolor:0x1d2c4d
style=feature:poi.park%7Celement:geometry.fill%7Ccolor:0x023e58
style=feature:poi.park%7Celement:labels.text.fill%7Ccolor:0x3C7680

style=feature:road%7Cvisibility:off
style=feature:road%7Celement:geometry%7Ccolor:0x304a7d
style=feature:road%7Celement:labels.icon%7Cvisibility:off
style=feature:road%7Celement:labels.text.fill%7Ccolor:0x98a5be
style=feature:road%7Celement:labels.text.stroke%7Ccolor:0x1d2c4d
style=feature:road.highway%7Celement:geometry%7Ccolor:0x2c6675
style=feature:road.highway%7Celement:geometry.stroke%7Ccolor:0x255763
style=feature:road.highway%7Celement:labels.text.fill%7Ccolor:0xb0d5ce
style=feature:road.highway%7Celement:labels.text.stroke%7Ccolor:0x023e58

style=feature:transit%7Cvisibility:off
style=feature:transit%7Celement:labels.text.fill%7Ccolor:0x98a5be
style=feature:transit%7Celement:labels.text.stroke%7Ccolor:0x1d2c4d
style=feature:transit.line%7Celement:geometry.fill%7Ccolor:0x283d6a
style=feature:transit.station%7Celement:geometry%7Ccolor:0x3a4762

style=feature:water%7Celement:geometry%7Ccolor:#0e1626
style=feature:water%7Celement:labels.text.fill%7Ccolor:0x4e6d70&size=480x360