foundyou
========

Straightforward library for using Google's geocode api.  Check the docstrings for geocode and reverse-geocode for extensive info on its usage.

*NOTE: You do* ***not*** *need a developer key from Google to use this library.*

## Examples

In order to geocode an address, call ```foundyou:geocode```

    CL-USER> (foundyou:geocode "Havenkant 2a, 3000 Leuven")
    (15902323/312500 4700311399999999/1000000000000000)
    9
    "Tussen Twee Waters, Havenkant 2, 3000 Leuven, Belgium"

Inorder to reverse geocode a coordinate, call ```foundyou:reverse-geocode```

    CL-USER> (foundyou:reverse-geocode 15902323/312500 4700311399999999/1000000000000000)
    (:POSTAL-CODE "3000" :COUNTRY "Belgium" :AREA-LEVEL-1 "Vlaams Gewest"
     :AREA-LEVEL-2 "Vlaams-Brabant" :CITY "Leuven" :STREET-NAME "Havenkant"
     :STREET-NUMBER "2")
    (:POSTAL-CODE "3000" :COUNTRY "BE" :AREA-LEVEL-1 "Vlaams Gewest" :AREA-LEVEL-2
     "VB" :CITY "Leuven" :STREET-NAME "N26a" :STREET-NUMBER "2")
    "Tussen Twee Waters, Havenkant 2, 3000 Leuven, Belgium"

For more info, check the extensive docstrings.
