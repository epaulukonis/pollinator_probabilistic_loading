#pip install earthengine-api

# import ee
# import os
# ee.Authenticate()
# ee.Initialize()
# print(ee.Image("NASA/NASADEM_HGT/001").get("title").getInfo())

# #make sure that the pyhon GEE API is ready
# image = ee.Image("NASA/NASADEM_HGT/001")
# print(image.getInfo())

#this is the part I can't get to run...
python ee_Landsat_LAI_export_v0.1.1.py -o projects/ee-elizabethpaulukonis/assets/LAI_test -p 44 -r 33 -s 2020-06-01 -e 2020-06-15

