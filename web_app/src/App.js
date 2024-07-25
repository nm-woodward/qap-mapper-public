import React, { useRef, useEffect, useState } from "react";
import MapboxGeocoder from "@mapbox/mapbox-gl-geocoder";
import "@mapbox/mapbox-gl-geocoder/dist/mapbox-gl-geocoder.css";
import mapboxgl from "mapbox-gl";
import "mapbox-gl/dist/mapbox-gl.css";
import "./App.css";
import axios from "axios";

import ScoringData from "./ScoringData";

mapboxgl.accessToken =
  "your_mapbox_access_token";

export default function App() {
  const mapContainer = useRef(null);
  const map = useRef(null);
  const [lng, setLng] = useState(-84.388);
  const [lat, setLat] = useState(33.749);
  const [zoom, setZoom] = useState(10);
  const [coordinates, setCoordinates] = useState(null);
  const [data, setData] = useState(null);
  const markers = useRef([]);

  useEffect(() => {
    if (map.current) return; // initialize map only once
    map.current = new mapboxgl.Map({
      container: mapContainer.current,
      style: "mapbox://styles/mapbox/light-v11",
      center: [lng, lat],
      zoom: zoom,
    });

    map.current.on("move", () => {
      setLng(map.current.getCenter().lng.toFixed(4));
      setLat(map.current.getCenter().lat.toFixed(4));
      setZoom(map.current.getZoom().toFixed(2));
    });

    const geocoder = new MapboxGeocoder({
      accessToken: mapboxgl.accessToken,
      mapboxgl: mapboxgl,
      countries: "usa",
      placeholder: "Enter site address",
    });

    geocoder.on("result", (e) => {
      const { geometry } = e.result;
      setCoordinates(geometry.coordinates);
    });

    document
      .getElementById("geocoder")
      .appendChild(geocoder.onAdd(map.current));
  }, []);

  // Effect to trigger the API call when coordinates change
  useEffect(() => {
    if (coordinates) {
      const fetchData = async () => {
        try {
          const response = await axios.get("http://localhost:8000/score", {
            params: {
              site_latitude: coordinates[1],
              site_longitude: coordinates[0],
              application_year: 2024,
              api_key: "your_google_api_key",
            },
          });

          setData(response.data);
          console.log(response.data);

          // Add markers to map here
          // Clear existing markers
          markers.current.forEach((marker) => marker.remove());
          markers.current = [];

          // Add new markers to the map
          response.data.scored_activity_table.forEach((activity) => {
            if (activity.site_latitude && activity.site_longitude) {
              const marker = new mapboxgl.Marker()
                .setLngLat([activity.site_longitude, activity.site_latitude])
                .setPopup(
                  new mapboxgl.Popup({ offset: 25 }).setText(
                    activity.qapDesirableActivity + ": " + activity.name
                  )
                )
                .addTo(map.current);

              markers.current.push(marker);
            }
          });
        } catch (error) {
          console.error("Error fetching data", error);
        }
      };

      fetchData();
    }
  }, [coordinates]);

  return (
    <div>
      <h1 className="title-header">Georgia QAP Site Scorer</h1>
      <div ref={mapContainer} className="map-container" />
      <div id="geocoder" className="geocoder-container"></div>
      {coordinates && <ScoringData data={data} />}
    </div>
  );
}
