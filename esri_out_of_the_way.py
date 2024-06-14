import json, os, requests, time


class ESRIInTheWayException(Exception): pass


class ESRIScraper():
    def __init__(self):
        self.session = requests.sessions.Session()
        self.headers = {
            'user-agent': "Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:127.0) Gecko/20100101 Firefox/127.0"
        }
        self.post_data = {
            "where": "1=1",
            "outFields": "OBJECTID,offshorest,Structure,widebeach,SAV,bathymetry,beach,marsh_all,RiparianLU,bnk_height,exposure,tribs,roads,PermStruc,canal,SandSpit,PublicRamp,defended,Fetch_,SMMv5Class,SMMv5Def,AdditionalInfo,PermittingInfo,Shape__Length",
            "f": "pgeojson",
            "geometryType": "esriGeometryEnvelope",
            "spatialRel": "esriSpatialRelIntersects",
            "distance": "0.0",
            "units": "esriSRUnit_Meter",
            "returnGeodetic": "false",
            "returnGeometry": "true",
            "returnEnvelope": "false",
            "featureEncoding": "esriDefault",
            "multipatchOption": "xyFootprint",
            "applyVCSProjection": "false",
            "returnIdsOnly": "false",
            "returnUniqueIdsOnly": "false",
            "returnCountOnly": "false",
            "returnExtentOnly": "false",
            "returnQueryGeometry": "false",
            "returnDistinctValues": "false",
            "cacheHint": "false",
            "returnZ": "false",
            "returnM": "false", 
            "returnExceededLimitFeatures": "true",
            "sqlFormat": "none",
            "objectIds": "",
            "time": "",
            "geometry": "",
            "inSR": "",
            "resultType": "none",
            "relationParam": "",
            "maxAllowableOffset": "",
            "geometryPrecision": "",
            "outSR": "",
            "defaultSR": "",
            "datumTransformation": "",
            "orderByFields": "",
            "groupByFieldsForStatistics": "",
            "outStatistics": "",
            "having": "",
            "resultOffset": "",
            "resultRecordCount": "",
            "quantizationParameters": "", 
            "token": ""
    }
    
    def pull_app_info(self, appid):
        url = "https://www.arcgis.com/sharing/rest/content/items/"
        url += f"{appid}/data"
        r = self.session.get(url, headers=self.headers)
        if r.status_code >= 400:
            raise ESRIInTheWayException(
                f"GET {url} returned {r.status_code} - {r.reason}"
            )
        else:
            return json.loads(r.content.decode())
    
    def get_count(self, url):
        post_args = self.post_data.copy()
        post_args['returnCountOnly'] = "true" 

        response = self.session.post(f"{url}/query", data=post_args, headers=self.headers)
        content = json.loads(response.content.decode())
        return content['properties']['count']

    def get_webmaps(self, appid):
        appinfo = self.pull_app_info(appid)
        return self.pull_app_info(appinfo['values']['webmap'])

    def check_layers(self, appid, sleep=2):
        webmap_info = self.get_webmaps(appid)
        for webmap in webmap_info['operationalLayers']:
            count = self.get_count(webmap['url'])
            time.sleep(sleep)
            print(f"{webmap['title']} has {count} entries.")
    
    def pull_data(self, url, dir_path, file_path, total_observations=None, limit=2000, sleep=2, path="./Data"):
        if not total_observations:
            total_observations = int(self.get_count(url))
        if not os.path.exists(os.path.join(path, dir_path)):
            os.mkdir(os.path.join(path, dir_path))
        path = os.path.join(path, dir_path, file_path)
        data = {}
        post_args = self.post_data.copy()
        objectid_limit = 0
        next_limit = 0
        while objectid_limit < total_observations:        
            next_limit += limit-1
            if next_limit > total_observations:
                print(f"Pulling where OBJECTID > {objectid_limit}")
                post_args["where"] = f"OBJECTID > {objectid_limit}"
            else:
                print(f"Pulling where OBJECTID BETWEEN {objectid_limit} AND {next_limit}")
                post_args["where"] = f"OBJECTID BETWEEN {objectid_limit} AND {next_limit}"
            objectid_limit = next_limit
            response = self.session.post(f"{url}/query", data=post_args)
            pulled_data = json.loads(response.content.decode())
            if data:
                data['features'].extend(pulled_data['features'])
            else:
                data.update(pulled_data)
            print(f"Currently at {len(data['features'])} features")
            print(f"Sleeping for {sleep} seconds...")
            time.sleep(sleep)
        with open(f"{path}.geojson", 'w') as f:
            json.dump(data, f)
            print(f"Dumped data to {path}.geojson")
        return data

    def check_out_fields(self, url, sleep=2):
        accepted = []
        failed = []
        post_data = self.post_data.copy()
        out_fields = post_data['outFields'].split(',')
        for out_field in out_fields:
            next_attempt = ','.join(accepted + [out_field])
            print(f"Out field={next_attempt}")
            post_data['outFields'] = next_attempt
            r = self.session.post(f"{url}/query", data=post_data)
            if "Unable to perform" in r.content.decode():
                failed.append(out_field)
            else:
                accepted.append(out_field)
                time.sleep(2)
        return ','.join(accepted)