#!/usr/bin/env python
# -*- coding: utf-8 -*-
import xml.etree.cElementTree as ET
import collections as collections
import pprint
import re
import codecs
import json

import pymongo
import pprint

# https://docs.python.org/2/library/collections.html

OSMFILE = "sample.osm"
street_type_re = re.compile(r'\b\S+\.?$', re.IGNORECASE)

lower = re.compile(r'^([a-z]|_)*$')
lower_colon = re.compile(r'^([a-z]|_)*:([a-z]|_)*$')
problemchars = re.compile(r'[=\+/&<>;\'"\?%#$@\,\. \t\r\n]')

expected_streetnames = ["Street", "Avenue", "Boulevard", "Drive", "Court", 
            "Place", "Square", "Lane", "Road", "Trail", "Parkway", "Commons", 
            "Way", "Loop", "Circle"]

CREATED = [ "version", "changeset", "timestamp", "user", "uid"]

mapping = { "St": "Street",
            "St.": "Street",
            "Rd.": "Road",
            "Rd": "Road",
            "Ave" : "Avenue",
            "Hwy. 9" : "Highway 9",
            "Hwy 9" : "Highway 9",
            "WAy" : "Way"
            }

def update_name(name, mapping):
	# will only match end
    for k in mapping.keys():
        name = re.sub(k + "$", mapping[k], name)
	
    return name

def update_postcode(postcode):
    postcode = postcode.replace("CA", "").strip() # remove CA
    postcode = postcode.split("-")[0] # remove trailing digits
    return postcode

def shape_element(element):
    node = {}
    if element.tag == "node" or element.tag == "way" :
        node["type"] = element.tag
        for key in element.attrib:
            if key in CREATED:
                if not "created" in node:
                    node["created"] = {}
                node["created"][key] = element.attrib[key]
            elif key == "lat":
                node["pos"] = [float(element.attrib["lat"]), float(element.attrib["lon"])]
            elif key == "lon":
                continue
            else:
                node[key] = element.attrib[key]
        for child in element:
            for ckey in child.attrib:
                if ckey == "ref" and child.tag == "nd" and element.tag == "way":
                    if not "node_refs" in node:
                        node["node_refs"] = []
                    node["node_refs"].append(child.attrib[ckey])
                elif ckey == "k":
                    if re.search(problemchars, child.attrib[ckey]):
                        continue # skip
                    if re.search("^addr:", child.attrib[ckey]):
                        if re.search("^addr:.*:", child.attrib[ckey]):
                            continue # skip multiple :
                        if not "address" in node:
                            node["address"] = {}
                        if child.attrib[ckey] == "addr:street":
                            child.attrib['v'] = update_name(child.attrib['v'], mapping)
                        if child.attrib[ckey] == "addr:postcode":
                            child.attrib['v'] = update_postcode(child.attrib['v'])
                        node["address"][re.sub("addr:", "", child.attrib[ckey])] = child.attrib['v']
                    else:
                        node[child.attrib[ckey]] = child.attrib['v']
                elif ckey == "v":
                    continue # skip
                else:
                    node[ckey] = child.attrib[ckey]
        return node
    else:
        return None

def process_map(file_in, pretty = False):
    data = []
    file_out = "{0}.json".format(file_in.split(".osm")[0])
    with codecs.open(file_out, "w") as fo:
        for _, element in ET.iterparse(file_in):
            el = shape_element(element)
            if el:
                data.append(el)
                if pretty:
                    fo.write(json.dumps(el, indent=2)+"\n")
                else:
                    fo.write(json.dumps(el) + "\n")
    return data

def count_attrib(osmfile):
    # Count the number of different tag atttributes of type 'node' and 'way'
    osm_file = open(osmfile, "r")
    total = collections.Counter()
    
    for event, elem in ET.iterparse(osm_file, events=("start",)):
        
        if elem.tag == "node" or elem.tag == "way":
            for tag in elem.iter("tag"):
                total[tag.attrib['k']] += 1
	
    return total

def count_elements(osmfile):
    # Count the number of differnt types of elements within osm sample
    osm_file = open(osmfile, "r")
    total = collections.Counter()
    
    for event, elem in ET.iterparse(osm_file, events=("start",)):
        
        total[elem.tag] += 1
	
    return total

def count_postalcodes(osmfile):
    # Count the number of differnt zip codes within osm sample
    osm_file = open(osmfile, "r")
    total = collections.Counter()
    
    for event, elem in ET.iterparse(osm_file, events=("start",)):
        if elem.tag == "node" or elem.tag == "way":
            for tag in elem.iter("tag"):
                if tag.attrib['k'] == "addr:postcode":
                    total[tag.attrib['v']] += 1
	
    return total

def count_addrstreet(osmfile):
    # Count the number of entries for each street (find the streets with most annotations)
    osm_file = open(osmfile, "r")
    total = collections.Counter()
    
    for event, elem in ET.iterparse(osm_file, events=("start",)):
        if elem.tag == "node" or elem.tag == "way":
            for tag in elem.iter("tag"):
                if tag.attrib['k'] == "addr:street":
                    total[tag.attrib['v']] += 1
	
    return total

def count_blankaddr(osmfile):
    # Count the number of entries with street XOR housenumber
    osm_file = open(osmfile, "r")
    total = collections.Counter()
    
    for event, elem in ET.iterparse(osm_file, events=("start",)):
        if elem.tag == "node" or elem.tag == "way":
            has_street = False
            has_housenum = False
            tmp_name = "None"
            for tag in elem.iter("tag"):
                if tag.attrib['k'] == "addr:street":
                    has_street = True
                if tag.attrib['k'] == "addr:housenumber":
                    has_housenum = True
                if tag.attrib['k'] == "name":
                    tmp_name = tag.attrib['v']
            # http://stackoverflow.com/questions/432842/how-do-you-get-the-logical-xor-of-two-variables-in-python
            if bool(has_street) ^ bool(has_housenum): # xor
                total[elem.attrib['id']] += 1
	
    return total

def test():
    # code below will count the different elements of interest
    # for a couple, not all edge cases were found in the sample so query was run against full dataset

    print("Number of different attributes:")
    count_attrib("sample.osm")
    # Counter({'highway': 1379, 'name': 1155, 'tiger:county': 452, 'tiger:cfcc': 445, 'building': 437, 'tiger:name_base': 379, 'tiger:name_type': 362, 'created_by': 351, 'amenity': 329, 'tiger:zip_left': 304, 'tiger:reviewed': 292, 'tiger:zip_right': 285, 'landuse': 216, 'tiger:tlid': 199, 'tiger:source': 198, 'source': 196, 'tiger:separated': 186, 'crossing': 185, 'emergency': 173, 'service': 160, 'natural': 138, 'access': 132, 'ID': 124, 'wheelchair': 124, 'bicycle': 123, 'attribution': 113, 'oneway': 93, 'note': 79, 'Shape_len': 78, 'Shape_area': 78, 'addr:housenumber': 77, 'waterway': 72, 'leisure': 69, 'addr:street': 69, 'entrance': 67, 'Zone': 63, 'shop': 62, 'Atribution': 59, 'addr:postcode': 55, 'cycleway': 55, 'addr:city': 54, 'tiger:upload_uuid': 52, 'src:id': 50, 'parking': 45, 'intermittent': 45, 'SHAPE_area': 44, 'SHAPE_len': 44, 'barrier': 43, 'surface': 43, 'operator': 42, 'tourism': 40, 'footway': 39, 'GENERIC': 36, 'Zoning': 35, 'SHAPE_STLe': 35, 'SHAPE_STAr': 35, 'OBJECTID': 35, 'foot': 33, 'power': 32, 'bridge': 32, 'maxspeed': 31, 'Attribution': 31, 'ele': 31, 'layer': 31, 'tiger:name_base_1': 30, 'description': 30, 'lanes': 28, 'cuisine': 28, 'sport': 27, 'railway': 27, 'capacity': 26, 'ref': 26, 'button_operated': 24, 'gnis:feature_id': 23, 'addr:state': 23, 'man_made': 22, 'tracktype': 22, 'gnis:created': 21, 'level': 20, 'gnis:state_id': 19, 'alt_name': 19, 'tiger:name_type_1': 19, 'gnis:county_id': 19, 'dog': 19, 'horse': 18, 'phone': 18, 'tiger:zip_left_1': 17, 'shelter': 17, 'traffic_calming': 16, 'bench': 14, 'website': 14, 'sloped_curb': 14, 'tactile_paving': 14, 'park:type': 13, 'name_1': 13, 'information': 12, 'sac_scale': 11, 'fee': 11, 'hgv': 11, 'building:levels': 11, 'hgv:national_network': 10, 'source:hgv:national_network': 10, 'religion': 10, 'usage': 9, 'area': 9, 'opening_hours': 9, 'gns:id': 8, 'place': 8, 'type': 8, 'history': 8, 'golf': 7, 'tiger:zip_right_1': 7, 'motor_vehicle': 7, 'fixme': 6, 'capacity:disabled': 5, 'denomination': 5, 'incline': 5, 'FIXME': 5, 'psv': 4, 'historic': 4, 'gnis:county_name': 4, 'tiger:zip_left_2': 4, 'tiger:name_direction_prefix': 4, 'tiger:name_base_2': 4, 'NHS': 4, 'mooring': 4, 'floating': 4, 'gnis:ST_num': 3, 'gnis:id': 3, 'is_in': 3, 'lit': 3, 'covered': 3, 'junction': 3, 'gnis:County': 3, 'motorcycle': 3, 'import_uuid': 3, 'gnis:Class': 3, 'gnis:County_num': 3, 'hiking': 3, 'addr:housename': 3, 'park_ride': 3, 'gnis:import_uuid': 3, 'bicycle_parking': 3, 'source:name': 3, 'gnis:ST_alpha': 3, 'proposed': 2, 'tiger:zip_right_2': 2, 'seamark:type': 2, 'wood': 2, 'trail_visibility': 2, 'seamark:reference': 2, 'dist:white': 2, 'public_transport': 2, 'disused': 2, 'recycling:plastic_bottles': 2, 'railway:proposed': 2, 'boundary': 2, 'seamark:name': 2, 'motorcar': 2, 'recycling:glass_bottles': 2, 'addr:unit': 2, 'ford': 2, 'dist:blue': 2, 'dispensing': 2, 'male': 2, 'par': 2, 'handicap': 2, 'dist:red': 2, 'addr:country': 2, 'step_count': 1, 'wetap:photo': 1, 'tiger:zip_right_3': 1, 'terrace': 1, 'Open_Date': 1, 'payment:card': 1, 'office': 1, 'gnis:feature_type': 1, 'recycling:aluminium': 1, 'note3': 1, 'seamark:buoy_lateral:system': 1, 'lot_no': 1, 'zoning_code': 1, 'loc_name': 1, 'note_1': 1, 'caltrans:district': 1, 'construction': 1, 'seamark:buoy_lateral:shape': 1, 'recycling:magazines': 1, 'recycling:newspaper': 1, 'smoothness': 1, 'disabled': 1, 'CertID': 1, 'payment:credit_cards': 1, 'addr:phone': 1, 'seamark:light:character': 1, 'sidewalk': 1, 'seamark:landmark:category': 1, 'recycling:paper': 1, 'seamark:landmark:colour': 1, 'seamark:light:period': 1, 'source:pkey': 1, 'id': 1, 'mtb:scale:uphill': 1, 'seamark:fog_signal:period': 1, 'wpt_description': 1, 'recycling:paper_packaging': 1, 'wikipedia': 1, 'factory': 1, 'seamark:buoy_lateral:category': 1, 'seamark:light:height': 1, 'source:website': 1, 'addr:full': 1, 'cycleway:left': 1, 'Shape_Area': 1, 'traffic_signals:sound': 1, 'tiger:zip_left_3': 1, 'noexit': 1, 'monitoring:water_level': 1, 'jams': 1, '_Shape_Leng_': 1, 'route': 1, 'tiger:name_direction_suffix': 1, 'second_hand': 1, 'swimming': 1, 'comment': 1, 'wetap:flow': 1, 'recycling:glass': 1, 'crossing_ref': 1, 'mtb:scale': 1, 'services': 1, 'female': 1, 'boat': 1, 'CertStat': 1, 'recycling:cartons': 1, 'note:highway': 1, 'destination': 1, '_Shape_Area_': 1, 'wetap:temperature': 1, 'emergency_service': 1, 'seamark:light:colour': 1, 'railway:name': 1, '_Acres_': 1, 'seamark:buoy_lateral:colour': 1, 'fuel:diesel': 1, 'recycling:plastic_packaging': 1, 'addr:county': 1, 'organic': 1, 'quantity': 1, 'tunnel': 1, 'car': 1, 'levels': 1, 'gnis:reviewed': 1, 'seamark:fog_signal:category': 1, 'seamark:light:range': 1, 'recycling:plastic': 1, 'wetap:quality': 1, 'seamark': 1, 'width': 1, 'traffic_signal:sound': 1, 'cables': 1, 'recycling:cardboard': 1, 'recycling_type': 1, 'recycling:cans': 1, 'designation': 1, 'supervised': 1, 'border_type': 1, 'note_2': 1, 'wires': 1, 'caltrans:pctuse': 1, 'fire_hydrant:position': 1, 'fence_type': 1, 'hazard': 1, 'buoy:colour': 1, 'note2': 1, 'maxspeed:hgv': 1, 'fuel:HGV_diesel': 1, 'wetap:status': 1, 'traffic_signals:direction': 1, 'payment:coins': 1, 'wifi': 1, 'craft': 1, 'truck': 1, 'bottle': 1, 'buoy': 1})

    print("Number of different types:")
    count_elements("sample.osm")
    # Counter({'nd': 30871, 'node': 25767, 'tag': 12011, 'way': 2103, 'member': 394, 'relation': 44, 'osm': 1})

    print("Number of different postal codes: (complete dataset)")
    count_postalcodes("santa-cruz_california.osm")
    # Counter({'95064': 396, '95060': 75, '95062': 10, '95018': 8, '95003': 7, '95073': 6, '95066': 6, '95010': 6, '95065': 3, '1982': 1, '95065-1711': 1, '95002': 1, '95041': 1, '95066-5121': 1, '95062-4205': 1, 'CA 95062': 1, '95066-4024': 1})

    print("Number of different streets: (complete dataset)")
    count_addrstreet("santa-cruz_california.osm")
    # Counter({'Porter-Kresge Road': 35, 'Koshland Way': 26, 'Village Road': 24, 'Red Hill Road': 20, 'Stevenson Service Road': 19, 'Nobel Drive': 18, 'McAllister Way': 18, 'East Road': 16, 'Farm Road': 14, 'College Eight Service Road': 14, 'Mount Hermon Road': 14, 'Cowell-Stevenson Road': 14, 'Kerr Road': 14, 'Steinhart Way': 14, 'Heller Drive': 13, 'Emeline Avenue': 13, 'Carriage House Road': 12, 'Baskin Arts Service Road': 12, 'Oakes Road': 11, 'Merrill Road': 10, 'Pacific Avenue': 10, 'College Ten Road': 10, 'Soquel Avenue': 10, 'Arboretum Road': 9, 'Farm Fire Road': 9, 'Crown Road': 9, 'Crown Service Road': 9, 'Cowell Service Road': 9, 'Mission Street': 8, 'Soquel Drive': 8, 'Esplanade': 8, 'College Eight Road': 8, 'McHenry Road': 8, 'Oakes Field Service Road': 7, 'Graham Hill Road': 7, 'Scotts Valley Drive': 6, 'College Nine Road': 6, 'Ox Team Road': 6, 'Lincoln Street': 6, 'Hagar Drive': 6, 'East Field Service Road': 5, 'Glenn Coolidge Drive': 5, 'River Street': 5, 'Highway 9': 4, 'Green Hills Road': 4, 'Pacific': 4, 'Fuel Break Road': 3, 'Bay Avenue': 3, 'East Cliff Drive': 3, '41st Avenue': 3, 'Center Street': 3, 'Ranch View Road': 3, 'Fair Avenue': 3, 'Delaware Avenue': 3, 'Capitola Avenue': 3, 'La Madrona Drive': 2, 'McLaughlin Drive': 2, 'Baskin Circle': 2, '41st Ave': 2, 'Ingalls Street': 2, '3rd Street': 2, 'Front Street': 2, 'Beach Drive': 2, 'Mission Street Extension': 2, 'High Street': 2, 'Main Street': 2, 'Monterey Avenue': 2, 'Morrissey Boulevard': 2, 'Church Street': 2, 'Hagar Court': 2, 'Engineering Loop': 2, 'Cooper Street': 1, 'Merrill Service Road': 1, 'Bostwick Lane': 1, 'Atlantic Avenue': 1, 'Seabright': 1, 'Rodeo Creek Gulch': 1, 'South Navarra Drive': 1, 'Water Street': 1, 'West Cliff Drive': 1, 'front': 1, 'Bean Creek Road': 1, '220 Sylvania Ave': 1, 'East Zayante Road': 1, 'Chanticleer Ave': 1, 'Chestnut': 1, 'Foundry Road': 1, '@ Pasatiempo Sb Ramps': 1, 'Cedar Street': 1, '245': 1, 'Golf Club Drive': 1, 'Wolverine Way': 1, 'Canham Road': 1, 'Merrill': 1, 'Hahn Road': 1, 'Lockhardt Gulch Road': 1, 'Central Avenue': 1, '7th Avenue': 1, 'Woodrow Avenue': 1, 'Conference Drive': 1, 'Hwy. 9': 1, 'El Pueblo Drive': 1, 'Riverside Avenue': 1, 'Sunridge Drive': 1, 'Walnut Avenue': 1, 'Cedar': 1, '7600 Soquel Drive': 1, 'Plymouth Street': 1, 'Chinquapin Road': 1, 'Front': 1, 'Rancho Del Mar': 1, 'Ocean Street': 1, '225 Rooney St': 1, 'Chanticleer Avenue': 1, '24th Avenue': 1, 'Enterprise Way': 1, 'Harvey West Boulevard': 1, 'Porter Street': 1, 'Mount Hermon Rd': 1, 'Pine Flat Road': 1, 'McAllister WAy': 1, 'Dubois Street': 1, 'Hwy 9': 1, 'Beach Street': 1, 'Vine Hill Road': 1, 'Glen Haven Road': 1, 'Leonardo Lane': 1, 'Old Dominion Court': 1, 'Swift Street': 1, 'Auto Plaza Drive': 1, 'Fifth Ave': 1, 'Gushee Street': 1, 'Wharf Road': 1, 'Chestnut Street': 1, 'Felt Street': 1, 'Union Street': 1, 'Coolidge Drive': 1, '30th Avenue': 1})

    print("Entry IDs with inconsistant addresses:")
    count_blankaddr("sample.osm")
    # Counter({'2610436148': 1, '35127761': 1, '2834887701': 1, '2604085615': 1, '58936115': 1, '2527138285': 1, '2372421676': 1, '493888676': 1, '2193128460': 1, '395025442': 1, '368174060': 1, '841482975': 1, '2354309925': 1, '2318703298': 1, '2283444567': 1, '2527134529': 1, '498428704': 1, '1507769627': 1, '2598866228': 1, '384862260': 1})

    
    #out = process_map("sample.osm", True) # can replace sample.osm with osm_file.name
    out = process_map("santa-cruz_california.osm", False) 
    
if __name__ == "__main__":
    test()