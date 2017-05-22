var buf_5km = ee.FeatureCollection('ft:18iqj8u-8r1Al3wIwYkjYrvRUXMhK7akhI4chptmS');
var buf_15km = ee.FeatureCollection('ft:1hDo8aPeIP71Ctv25jgDOzffhkjN_LQ_B1yFlkHfC');
var buf_25km = ee.FeatureCollection('ft:1uLO_0i0MddINBTaxwGWY6NvhQ8J8Q1pcvjxNVKTk');
//Map.addLayer(buf_5km);
//Map.addLayer(buf_15km);
//Map.addLayer(buf_25km);

// load forest cover
var forest_loss = ee.Image('UMD/hansen/global_forest_change_2015').select('loss');
var forest_gain = ee.Image('UMD/hansen/global_forest_change_2015').select('gain');
//Map.addLayer({eeObject:forest_loss, name:"hansen-forest-loss"});

// load GLS (USGS/NASA)
var gls_tree = ee.ImageCollection("GLCF/GLS_TCC")
               .filterDate('2010-01-01', '2010-12-31').mosaic()
               .select('tree_canopy_cover');
var gls_water = ee.ImageCollection("GLCF/GLS_WATER")
               .filterDate('2000-01-01', '2000-12-31').mosaic()
               .select('water');
//Map.addLayer({eeObject:gls_tree, name:"gls-tree"});
//Map.addLayer({eeObject:gls_water, name:"gls-water"});

// load EVI (Landsat) - i think a better choice than NDVI in this case
// this is clunky, but might enable different date ranges and reducers in future (excuse for my rubbish code)
var evi_2013 = ee.ImageCollection("LANDSAT/LC8_L1T_ANNUAL_EVI")
               .filterDate('2013-01-01', '2013-12-31').mean();
var evi_2014 = ee.ImageCollection("LANDSAT/LC8_L1T_ANNUAL_EVI")
               .filterDate('2014-01-01', '2014-12-31').mean();
var evi_2015 = ee.ImageCollection("LANDSAT/LC8_L1T_ANNUAL_EVI")
               .filterDate('2015-01-01', '2015-12-31').mean();
var evi_2016 = ee.ImageCollection("LANDSAT/LC8_L1T_ANNUAL_EVI")
               .filterDate('2016-01-01', '2016-12-31').mean();
var evi_2017 = ee.ImageCollection("LANDSAT/LC8_L1T_ANNUAL_EVI")
               .filterDate('2017-01-01', '2017-12-31').mean();

// test one evi ayer
//Map.addLayer({eeObject:evi_2013, name:"evi-2013"});

// load globcover (ESA)
var esa_globcover = ee.Image("ESA/GLOBCOVER_L4_200901_200912_V2_3")
                    .select('landcover');
//Map.addLayer({eeObject:esa_globcover, name:"globcover"});

// now calculate the buffer metrics
// function to do the calculation
var reduce_by_buffer = function(image_in, buffer) {
  return image_in.reduceRegions({
    collection: buffer,
    reducer: ee.Reducer.mean(),
    scale: 30
  });
}
var reduce_by_buffer_mode = function(image_in, buffer) {
  return image_in.reduceRegions({
    collection: buffer,
    reducer: ee.Reducer.mode(),
    scale: 30
  });
}
// function to do the export
var export_buffer_stats = function(buf_stats, out_name) {
  Export.table.toDrive({
  collection: buf_stats,
  description: out_name,
  folder: 'ebird',
  fileFormat: 'CSV'
});
}

// run the calcs
// forest
export_buffer_stats(reduce_by_buffer(forest_loss, buf_5km), 'forest_loss_5');
export_buffer_stats(reduce_by_buffer(forest_loss, buf_15km), 'forest_loss_15');
export_buffer_stats(reduce_by_buffer(forest_loss, buf_25km), 'forest_loss_25');

export_buffer_stats(reduce_by_buffer(forest_gain, buf_5km), 'forest_gain_5');
export_buffer_stats(reduce_by_buffer(forest_gain, buf_15km), 'forest_gain_15');
export_buffer_stats(reduce_by_buffer(forest_gain, buf_25km), 'forest_gain_25');

// gls_tree
export_buffer_stats(reduce_by_buffer(gls_tree, buf_5km), 'gls_tree_5');
export_buffer_stats(reduce_by_buffer(gls_tree, buf_15km), 'gls_tree_15');
export_buffer_stats(reduce_by_buffer(gls_tree, buf_25km), 'gls_tree_25');

// gls_water
export_buffer_stats(reduce_by_buffer(gls_water, buf_5km), 'gls_water_5');
export_buffer_stats(reduce_by_buffer(gls_water, buf_15km), 'gls_water_15');
export_buffer_stats(reduce_by_buffer(gls_water, buf_25km), 'gls_water_25');

// evi
export_buffer_stats(reduce_by_buffer(evi_2013, buf_5km), 'evi_2013_5');
export_buffer_stats(reduce_by_buffer(evi_2013, buf_15km), 'evi_2013_15');
export_buffer_stats(reduce_by_buffer(evi_2013, buf_25km), 'evi_2013_25');

export_buffer_stats(reduce_by_buffer(evi_2014, buf_5km), 'evi_2014_5');
export_buffer_stats(reduce_by_buffer(evi_2014, buf_15km), 'evi_2014_15');
export_buffer_stats(reduce_by_buffer(evi_2014, buf_25km), 'evi_2014_25');

export_buffer_stats(reduce_by_buffer(evi_2015, buf_5km), 'evi_2015_5');
export_buffer_stats(reduce_by_buffer(evi_2015, buf_15km), 'evi_2015_15');
export_buffer_stats(reduce_by_buffer(evi_2015, buf_25km), 'evi_2015_25');

export_buffer_stats(reduce_by_buffer(evi_2016, buf_5km), 'evi_2016_5');
export_buffer_stats(reduce_by_buffer(evi_2016, buf_15km), 'evi_2016_15');
export_buffer_stats(reduce_by_buffer(evi_2016, buf_25km), 'evi_2016_25');

export_buffer_stats(reduce_by_buffer(evi_2017, buf_5km), 'evi_2017_5');
export_buffer_stats(reduce_by_buffer(evi_2017, buf_15km), 'evi_2017_15');
export_buffer_stats(reduce_by_buffer(evi_2017, buf_25km), 'evi_2017_25');

// esa glob cover

export_buffer_stats(reduce_by_buffer_mode(esa_globcover, buf_5km), 'esa_globcover_5');
export_buffer_stats(reduce_by_buffer_mode(esa_globcover, buf_15km), 'esa_globcover_15');
export_buffer_stats(reduce_by_buffer_mode(esa_globcover, buf_25km), 'esa_globcover_25');

