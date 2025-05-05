#!/bin/bash

# 📂 Hlavní složka s výstupy simulací (včetně variant LAI)
OUTPUT_DIR="/run/media/vkolar/Svazek/data0078125_lai_scenarios"

echo "=========================================="
echo "✅ Spouštím zpracování dat průtoků..."
echo "=========================================="

# 🔄 Procházení všech podsložek rekurzivně
find "$OUTPUT_DIR" -type f -name "mRM_Fluxes_States.nc" | while read -r FILE; do
    DIR=$(dirname "$FILE")
    echo "📊 Zpracovávám $DIR..."

    SIMULATION_DIR="$DIR/simulace"
    mkdir -p "$SIMULATION_DIR"

    cd "$DIR" || continue

    echo "📊 Extrahuji průtoky pro specifické profily..."

    cdo -outputtab,date,lon,lat,value -remapnn,"lon=15.6782696_lat=49.3003845" mRM_Fluxes_States.nc > "$SIMULATION_DIR/brtnice468000.csv"
    cdo -outputtab,date,lon,lat,value -remapnn,"lon=16.21705020_lat=49.6392206" mRM_Fluxes_States.nc > "$SIMULATION_DIR/frysavka441500.csv"
    cdo -outputtab,date,lon,lat,value -remapnn,"lon=15.40632130_lat=49.3137753" mRM_Fluxes_States.nc > "$SIMULATION_DIR/jihlava463000.csv"
    cdo -outputtab,date,lon,lat,value -remapnn,"lon=15.49916_lat=49.38075" mRM_Fluxes_States.nc > "$SIMULATION_DIR/jihlava465000.csv"
    cdo -outputtab,date,lon,lat,value -remapnn,"lon=15.93483530_lat=49.2150769" mRM_Fluxes_States.nc > "$SIMULATION_DIR/jihlava469000.csv"
    cdo -outputtab,date,lon,lat,value -remapnn,"lon=16.18291930_lat=49.1030689" mRM_Fluxes_States.nc > "$SIMULATION_DIR/jihlava469500.csv"
    cdo -outputtab,date,lon,lat,value -remapnn,"lon=15.98553450_lat=49.4225289" mRM_Fluxes_States.nc > "$SIMULATION_DIR/oslava470000.csv"
    cdo -outputtab,date,lon,lat,value -remapnn,"lon=16.01247030_lat=49.3927303" mRM_Fluxes_States.nc > "$SIMULATION_DIR/oslava471000.csv"
    cdo -outputtab,date,lon,lat,value -remapnn,"lon=16.02544810_lat=49.3196033" mRM_Fluxes_States.nc > "$SIMULATION_DIR/oslava473000.csv"
    cdo -outputtab,date,lon,lat,value -remapnn,"lon=15.69137170_lat=48.9033407" mRM_Fluxes_States.nc > "$SIMULATION_DIR/dyje430000.csv"
    cdo -outputtab,date,lon,lat,value -remapnn,"lon=15.81909890_lat=48.8946231" mRM_Fluxes_States.nc > "$SIMULATION_DIR/dyje434000.csv"
    cdo -outputtab,date,lon,lat,value -remapnn,"lon=16.43764310_lat=48.7908632" mRM_Fluxes_States.nc > "$SIMULATION_DIR/dyje437000.csv"
    cdo -outputtab,date,lon,lat,value -remapnn,"lon=16.34372140_lat=49.115976" mRM_Fluxes_States.nc > "$SIMULATION_DIR/oslava474000.csv"
    cdo -outputtab,date,lon,lat,value -remapnn,"lon=15.9389940_lat=49.0704303" mRM_Fluxes_States.nc > "$SIMULATION_DIR/rokytna476000.csv"
    cdo -outputtab,date,lon,lat,value -remapnn,"lon=16.58058490_lat=49.5320475" mRM_Fluxes_States.nc > "$SIMULATION_DIR/svitava454000.csv"
    cdo -outputtab,date,lon,lat,value -remapnn,"lon=16.66988_lat=49.24134" mRM_Fluxes_States.nc > "$SIMULATION_DIR/svitava457000.csv"
    cdo -outputtab,date,lon,lat,value -remapnn,"lon=16.43968210_lat=49.2774573" mRM_Fluxes_States.nc > "$SIMULATION_DIR/svratka448000.csv"
    cdo -outputtab,date,lon,lat,value -remapnn,"lon=16.59147960_lat=49.1859963" mRM_Fluxes_States.nc > "$SIMULATION_DIR/svratka449000.csv"
    cdo -outputtab,date,lon,lat,value -remapnn,"lon=16.61626450_lat=49.0363107" mRM_Fluxes_States.nc > "$SIMULATION_DIR/svratka462000.csv"
    cdo -outputtab,date,lon,lat,value -remapnn,"lon=15.56722460_lat=49.0211304" mRM_Fluxes_States.nc > "$SIMULATION_DIR/zeletavka431000.csv"
    cdo -outputtab,date,lon,lat,value -remapnn,"lon=16.8537248_lat=48.8037157" mRM_Fluxes_States.nc > "$SIMULATION_DIR/dyje480500.csv"

    echo "✅ Data průtoků uložena do $SIMULATION_DIR"
done

echo "=========================================="
echo "🎉 Všechny výstupy zpracovány!"
echo "=========================================="

