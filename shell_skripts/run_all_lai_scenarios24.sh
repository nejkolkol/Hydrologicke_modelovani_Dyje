#!/bin/bash

# 📂 Hlavní složky
SOURCE_DIR="/run/media/vkolar/Svazek/meteo_data/pozorovana/1961=2024"
OUTPUT_DIR="/run/media/vkolar/Svazek/2018-2024_0.0078125_lai_scenarios"
MHM_DIR="/home/vkolar/Desktop/mhm_work/devel/mhm"
METEO_DIR="$MHM_DIR/Thaya_test/meteo_station_CHMI"
OUT_DEF_DIR="$MHM_DIR/Thaya_test/out_def"
LAI_VARIANTS_DIR="/run/media/vkolar/Svazek/LAI_versions/LAI"
LAI_TARGET_DIR="$MHM_DIR/Thaya_test/lai_v2"
NML_FILE="$MHM_DIR/mhm.nml"

# 🔍 Rozlišení simulace
LATLON_FILE="Thaya_test/latlon/latlon_0p0078125.nc"
RESOLUTION=0.0078125

# 📆 Časové období
PERIOD="2024"
START_Y=2018
START_M=1
START_D=1
END_Y=2024
END_M=12
END_D=31

# 🔧 Nastavení rozlišení v namelistu
echo "⚙️ Nastavuji rozlišení simulace ($RESOLUTION) a latlon soubor ($LATLON_FILE) v mhm.nml..."
sed -i -e "s|file_LatLon(1) *= *\".*\"|file_LatLon(1) = \"$LATLON_FILE\"|" \
       -e "s|resolution_Routing(1) *= *[0-9.]*|resolution_Routing(1) = $RESOLUTION|" \
       -e "s|resolution_Hydrology(1) *= *[0-9.]*|resolution_Hydrology(1) = $RESOLUTION|" \
       "$NML_FILE"

echo "=========================================="
echo "📁 Spouštím simulace pro pozorované klima"
echo "=========================================="

# 🔁 Iterace přes LAI varianty (kromě 'original')
for LAI_PATH in "$LAI_VARIANTS_DIR"/*; do
    if [ -d "$LAI_PATH" ] && [ -f "$LAI_PATH/lai.nc" ]; then
        LAI_VARIANT=$(basename "$LAI_PATH")

        if [[ "$LAI_VARIANT" == "original" ]]; then
            echo "⏩ Přeskakuji variantu: $LAI_VARIANT"
            continue
        fi

        echo "🌿 Používám LAI variantu: $LAI_VARIANT"

        # 📂 Vytvoření výstupní složky
        SCENARIO_OUTPUT="${OUTPUT_DIR}/${PERIOD}/${LAI_VARIANT}"
        mkdir -p "$SCENARIO_OUTPUT"

        # 📂 Kopírování LAI
        cp -f "$LAI_PATH/lai.nc" "$LAI_TARGET_DIR/lai.nc"

        # 📂 Kopírování NC vstupů
        cp -f "$SOURCE_DIR/pet.nc" "$METEO_DIR/pet.nc"
        cp -f "$SOURCE_DIR/pre.nc" "$METEO_DIR/pre.nc"
        cp -f "$SOURCE_DIR/tavg.nc" "$METEO_DIR/tavg.nc"

        # 🛠️ Úprava namelistu
        sed -i -e "s|warming_Days(1) *= *[0-9]*|warming_Days(1) = 0|" \
               -e "s|eval_Per(1)%yStart *= *[0-9]*|eval_Per(1)%yStart = $START_Y|" \
               -e "s|eval_Per(1)%mStart *= *[0-9]*|eval_Per(1)%mStart = $START_M|" \
               -e "s|eval_Per(1)%dStart *= *[0-9]*|eval_Per(1)%dStart = $START_D|" \
               -e "s|eval_Per(1)%yEnd *= *[0-9]*|eval_Per(1)%yEnd = $END_Y|" \
               -e "s|eval_Per(1)%mEnd *= *[0-9]*|eval_Per(1)%mEnd = $END_M|" \
               -e "s|eval_Per(1)%dEnd *= *[0-9]*|eval_Per(1)%dEnd = $END_D|" \
               "$NML_FILE"

        # ⏱️ Čas simulace
        START_TIME=$(date +%s)
        echo "⏳ Simulace začala: $(date)" > "$SCENARIO_OUTPUT/mhm.log"

        cd "$MHM_DIR"
        ./mhm >> "$SCENARIO_OUTPUT/mhm.log" 2>&1 || echo "⚠️ Chyba během mHM, ale pokračuji..."

        END_TIME=$(date +%s)
        ELAPSED=$((END_TIME - START_TIME))
        echo "✅ Simulace dokončena: $(date)" >> "$SCENARIO_OUTPUT/mhm.log"
        echo "⏱️ Čas simulace: $ELAPSED sekund" >> "$SCENARIO_OUTPUT/mhm.log"

        # 📂 Kopírování výstupů
        if [ -f "$OUT_DEF_DIR/mHM_Fluxes_States.nc" ] && [ -f "$OUT_DEF_DIR/mRM_Fluxes_States.nc" ]; then
            cp "$OUT_DEF_DIR/mHM_Fluxes_States.nc" "$SCENARIO_OUTPUT/"
            cp "$OUT_DEF_DIR/mRM_Fluxes_States.nc" "$SCENARIO_OUTPUT/"
        else
            echo "⚠️ Výstupní soubory chybí! Simulace selhala?" >> "$SCENARIO_OUTPUT/mhm.log"
        fi

        echo "🏁 Dokončeno: $PERIOD + $LAI_VARIANT"
        echo
    fi
done

echo "✅ Všechny simulace dokončeny!"

