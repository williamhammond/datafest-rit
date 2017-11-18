# William Hammond
import requests

zillow_id = 'X1-ZWz19aily4i4uj_anh8k'

def get_zestimate(address, city, state):
    url = "http://www.zillow.com/webservice/GetDeepSearchResults.htm"
    payload = {'zws-id':zillow_id, "address":address, "citystatezip":(city + "," + state)}
    return requests.get(url, params=payload)

r = get_zestimate("2114 Bigelow Ave", "Seattle", "WA")
print(r.url)
print(r.content)
