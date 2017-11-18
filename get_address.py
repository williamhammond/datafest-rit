import requests

GOOGLE_API_KEY = ""
CENSUS_API_KEY = ""

def get_address(longitude, latitude):
    url = "https://maps.googleapis.com/maps/api/geocode/json"
    payload = {"latlng":str(latitude)+","+str(longitude)}
    return requests.get(url, params=payload).json()["results"][0]["formatted_address"]


print get_address(-73.9599399197085, 40.714224)

