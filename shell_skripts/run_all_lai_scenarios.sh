#!/bin/bash

# 📂 Hlavní složky
SOURCE_DIR="/run/media/vkolar/Svazek/meteo_data/klimaticke_scenare"
OUTPUT_DIR="/run/media/vkolar/Svazek/data0078125_lai_scenarios"
MHM_DIR="/home/vkolar/Desktop/mhm_work/devel/mhm"
METEO_DIR="$MHM_DIR/Thaya_test/meteo_station_CHMI"
OUT_DEF_DIR="$MHM_DIR/Thaya_test/out_def"
LAI_VARIANTS_DIR="/run/media/vkolar/Svazek/LAI_versions/LAI"
LAI_TARGET_DIR="$MHM_DIR/Thaya_test/lai_v2"
NML_FILE="$MHM_DIR/mhm.nml"

# 🔍 Rozlišení simulace
LATLON_FILE="Thaya_test/latlon/latlon_0p0078125.nc"
RESOLUTION=0.0078125

# 📆 Časová období
declare -A START_Y START_M START_D END_Y END_M END_D
START_Y["2030"]=2010; START_M["2030"]=11; START_D["2030"]=01
END_Y["2030"]=2044;   END_M["2030"]=12;   END_D["2030"]=31
START_Y["2050"]=2030; START_M["2050"]=11; START_D["2050"]=01
END_Y["2050"]=2064;   END_M["2050"]=12;   END_D["2050"]=31
START_Y["2070"]=2050; START_M["2070"]=11; START_D["2070"]=01
END_Y["2070"]=2084;   END_M["2070"]=12;   END_D["2070"]=31
START_Y["2085"]=2065; START_M["2085"]=11; START_D["2085"]=01
END_Y["2085"]=2099;   END_M["2085"]=12;   END_D["2085"]=31

# 🔧 Nastavení rozlišení v namelistu
echo "⚙️ Nastavuji rozlišení simulace ($RESOLUTION) a latlon soubor ($LATLON_FILE) v mhm.nml..."
sed -i -e "s|file_LatLon(1) *= *\".*\"|file_LatLon(1) = \"$LATLON_FILE\"|" \
       -e "s|resolution_Routing(1) *= *[0-9.]*|resolution_Routing(1) = $RESOLUTION|" \
       -e "s|resolution_Hydrology(1) *= *[0-9.]*|resolution_Hydrology(1) = $RESOLUTION|" \
       "$NML_FILE"

# 🔁 Iterace přes scénáře
for DIR in "$SOURCE_DIR"/*; do
    if [ -d "$DIR" ]; then
        BASENAME=$(basename "$DIR")
        IFS='_' read -r PERIOD SCENARIO MODEL <<< "$BASENAME"

        if [[ -n "${START_Y[$PERIOD]}" ]]; then
            echo "=========================================="
            echo "📁 Spouštím scénář $BASENAME"
            echo "=========================================="

            # 🔁 Iterace přes varianty LAI (kromě 'original')
            for LAI_PATH in "$LAI_VARIANTS_DIR"/*; do
                if [ -d "$LAI_PATH" ] && [ -f "$LAI_PATH/lai.nc" ]; then
                    LAI_VARIANT=$(basename "$LAI_PATH")

                    # Přeskočení 'original'
                    if [[ "$LAI_VARIANT" == "original" ]]; then
                        echo "⏩ Přeskakuji variantu: $LAI_VARIANT"
                        continue
                    fi

                    echo "🌿 Používám LAI variantu: $LAI_VARIANT"

                    # 📂 Vytvoření výstupní složky
                    SCENARIO_OUTPUT="${OUTPUT_DIR}/${BASENAME}/${LAI_VARIANT}"
                    mkdir -p "$SCENARIO_OUTPUT"

                    # 📂 Kopírování LAI
                    cp -f "$LAI_PATH/lai.nc" "$LAI_TARGET_DIR/lai.nc"

                    # 📂 Kopírování NC vstupů
                    cp -f "$DIR/pet.nc" "$METEO_DIR/pet.nc"
                    cp -f "$DIR/pre.nc" "$METEO_DIR/pre.nc"
                    cp -f "$DIR/tavg.nc" "$METEO_DIR/tavg.nc"

                    # 🛠️ Úprava namelistu
                    sed -i -e "s|warming_Days(1) *= *[0-9]*|warming_Days(1) = 0|" \
                           -e "s|eval_Per(1)%yStart *= *[0-9]*|eval_Per(1)%yStart = ${START_Y[$PERIOD]}|" \
                           -e "s|eval_Per(1)%mStart *= *[0-9]*|eval_Per(1)%mStart = ${START_M[$PERIOD]}|" \
                           -e "s|eval_Per(1)%dStart *= *[0-9]*|eval_Per(1)%dStart = ${START_D[$PERIOD]}|" \
                           -e "s|eval_Per(1)%yEnd *= *[0-9]*|eval_Per(1)%yEnd = ${END_Y[$PERIOD]}|" \
                           -e "s|eval_Per(1)%mEnd *= *[0-9]*|eval_Per(1)%mEnd = ${END_M[$PERIOD]}|" \
                           -e "s|eval_Per(1)%dEnd *= *[0-9]*|eval_Per(1)%dEnd = ${END_D[$PERIOD]}|" \
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

                    echo "🏁 Dokončeno: $BASENAME + $LAI_VARIANT"
                    echo
                fi
            done
        else
            echo "⚠️ Přeskakuji $BASENAME (neznámé období)"
        fi
    fi
done

echo "✅ Všechny simulace dokončeny!"

