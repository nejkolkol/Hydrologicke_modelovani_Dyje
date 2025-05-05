#!/bin/bash

# 📂 Hlavní složka s výstupy simulací (včetně variant LAI)
OUTPUT_DIR="/run/media/vkolar/Svazek/2018-2024_0.0078125_lai_scenarios"

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

    cdo -outputtab,date,lon,lat,value -remapnn,"lon=16.8537248_lat=48.8037157" mRM_Fluxes_States.nc > "$SIMULATION_DIR/dyje480500.csv"

    echo "✅ Data průtoků uložena do $SIMULATION_DIR"
done

echo "=========================================="
echo "🎉 Všechny výstupy zpracovány!"
echo "=========================================="

