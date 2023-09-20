from PyQt5.QtCore import *
from qgis.core import *
from qgis.gui import *
import os
import sys

path_a = 'C:/Users/EPAULUKO/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/GitHub/pollinator_probabilistic_loading/data_in/MapData/NLCD/NLCD/NLCD_F'

layers = iface.layerTreeView().selectedLayers()
for layer in layers:
    file_name = path_a + layer.name() + '.tif'
    file_writer = QgsRasterFileWriter(file_name)
    pipe = QgsRasterPipe()
    provider = layer.dataProvider()

    if not pipe.set(provider.clone()):
        print ("Cannot set pipe provider")
        continue

    file_writer.writeRaster(
        pipe,
        provider.xSize(),
        provider.ySize(),
        provider.extent(),
        provider.crs())