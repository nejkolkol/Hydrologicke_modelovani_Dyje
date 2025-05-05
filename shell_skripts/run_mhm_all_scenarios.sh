#!/bin/bash

# 📂 Hlavní složky
SOURCE_DIR="/run/media/vkolar/Svazek/meteo_data/klimaticke_scenare"
OUTPUT_DIR="/run/media/vkolar/Svazek/data0078125_scenarios"
MHM_DIR="/home/vkolar/Desktop/mhm_work/devel/mhm"
METEO_DIR="$MHM_DIR/Thaya_test/meteo_station_CHMI"
OUT_DEF_DIR="$MHM_DIR/Thaya_test/out_def"
LAI_SOURCE="/run/media/vkolar/Svazek/LAI_versions/LAI/original/lai.nc"
LAI_TARGET_DIR="$MHM_DIR/Thaya_test/lai_v2"
NML_FILE="$MHM_DIR/mhm.nml"

# 🔍 Nastavení rozlišení simulace
LATLON_FILE="Thaya_test/latlon/latlon_0p0078125.nc"
RESOLUTION=0.0078125

# 📆 Definování časových období
declare -A START_Y START_M START_D END_Y END_M END_D
START_Y["2030"]=2010; START_M["2030"]=11; START_D["2030"]=01
END_Y["2030"]=2044;   END_M["2030"]=12;   END_D["2030"]=31
START_Y["2050"]=2030; START_M["2050"]=11; START_D["2050"]=01
END_Y["2050"]=2064;   END_M["2050"]=12;   END_D["2050"]=31
START_Y["2070"]=2050; START_M["2070"]=11; START_D["2070"]=01
END_Y["2070"]=2084;   END_M["2070"]=12;   END_D["2070"]=31
START_Y["2085"]=2065; START_M["2085"]=11; START_D["2085"]=01
END_Y["2085"]=2099;   END_M["2085"]=12;   END_D["2085"]=31

# 🔧 Nastavení rozlišení v hlavním namelistu (mhm.nml)
echo "⚙️ Nastavuji rozlišení simulace ($RESOLUTION) a latlon soubor ($LATLON_FILE) v mhm.nml..."
sed -i -e "s|file_LatLon(1) *= *\".*\"|file_LatLon(1) = \"$LATLON_FILE\"|" \
       -e "s|resolution_Routing(1) *= *[0-9.]*|resolution_Routing(1) = $RESOLUTION|" \
       -e "s|resolution_Hydrology(1) *= *[0-9.]*|resolution_Hydrology(1) = $RESOLUTION|" \
       "$NML_FILE"

# 🔄 **Iterace přes všechny scénáře**
for DIR in "$SOURCE_DIR"/*; do
    if [ -d "$DIR" ]; then
        # 📌 Získání názvu scénáře
        BASENAME=$(basename "$DIR")
        IFS='_' read -r PERIOD SCENARIO MODEL <<< "$BASENAME"

        # Ověření platnosti období
        if [[ -n "${START_Y[$PERIOD]}" ]]; then
            echo "=========================================="
            echo "🚀 Spouštím mHM pro scénář: $SCENARIO, období: $PERIOD, model: $MODEL"
            echo "=========================================="

            # 📂 Nastavení výstupní složky
            SCENARIO_OUTPUT="${OUTPUT_DIR}/${PERIOD}_${SCENARIO}_${MODEL}"
            mkdir -p "$SCENARIO_OUTPUT"

            # 🔄 Kopírování souboru lai.nc
            if [ -f "$LAI_SOURCE" ]; then
                echo "📂 Kopíruji lai.nc do $LAI_TARGET_DIR..."
                cp -f "$LAI_SOURCE" "$LAI_TARGET_DIR/lai.nc"
            else
                echo "⚠️ Varování: Soubor lai.nc ($LAI_SOURCE) nebyl nalezen!"
                continue
            fi

            # 🔄 Kopírování vstupních souborů
            echo "📂 Nahrazuji vstupní soubory pro $SCENARIO - $PERIOD - $MODEL..."
            cp -f "${DIR}/pet.nc" "$METEO_DIR/pet.nc"
            cp -f "${DIR}/pre.nc" "$METEO_DIR/pre.nc"
            cp -f "${DIR}/tavg.nc" "$METEO_DIR/tavg.nc"

            # 🛠️ Úprava namelist souboru (mhm.nml)
            echo "⚙️ Nastavuji evaluační období v mhm.nml..."
            sed -i -e "s|warming_Days(1) *= *[0-9]*|warming_Days(1) = 0|" \
                   -e "s|eval_Per(1)%yStart *= *[0-9]*|eval_Per(1)%yStart = ${START_Y[$PERIOD]}|" \
                   -e "s|eval_Per(1)%mStart *= *[0-9]*|eval_Per(1)%mStart = ${START_M[$PERIOD]}|" \
                   -e "s|eval_Per(1)%dStart *= *[0-9]*|eval_Per(1)%dStart = ${START_D[$PERIOD]}|" \
                   -e "s|eval_Per(1)%yEnd *= *[0-9]*|eval_Per(1)%yEnd = ${END_Y[$PERIOD]}|" \
                   -e "s|eval_Per(1)%mEnd *= *[0-9]*|eval_Per(1)%mEnd = ${END_M[$PERIOD]}|" \
                   -e "s|eval_Per(1)%dEnd *= *[0-9]*|eval_Per(1)%dEnd = ${END_D[$PERIOD]}|" \
                   "$NML_FILE"

            # ⏱️ Zaznamenání času začátku
            START_TIME=$(date +%s)
            echo "⏳ Simulace začala: $(date)" > "$SCENARIO_OUTPUT/mhm.log"

            # 🚀 Spuštění modelu mHM
            echo "🚀 Spouštím model mHM pro $SCENARIO - $PERIOD - $MODEL..."
            cd "$MHM_DIR"
            ./mhm >> "$SCENARIO_OUTPUT/mhm.log" 2>&1 || echo "⚠️ Chyba během mHM, ale pokračuji..."

            # ⏳ Zaznamenání času konce
            END_TIME=$(date +%s)
            ELAPSED_TIME=$((END_TIME - START_TIME))
            echo "✅ Simulace dokončena: $(date)" >> "$SCENARIO_OUTPUT/mhm.log"
            echo "⏱️ Celkový čas simulace: $ELAPSED_TIME sekund" >> "$SCENARIO_OUTPUT/mhm.log"

            # ✅ Kopírování výstupních souborů
            if [ -f "$OUT_DEF_DIR/mHM_Fluxes_States.nc" ] && [ -f "$OUT_DEF_DIR/mRM_Fluxes_States.nc" ]; then
                echo "📁 Kopíruji výstupní soubory do $SCENARIO_OUTPUT..."
                cp "$OUT_DEF_DIR/mHM_Fluxes_States.nc" "$SCENARIO_OUTPUT/"
                cp "$OUT_DEF_DIR/mRM_Fluxes_States.nc" "$SCENARIO_OUTPUT/"
            else
                echo "⚠️ Varování: Výstupní soubory nejsou k dispozici! Simulace možná selhala!"
            fi

            echo "🏁 Scénář $SCENARIO - $PERIOD - $MODEL dokončen!"
        else
            echo "⚠️ Přeskočeno: $BASENAME (neznámé časové období)"
        fi
    fi
done

echo "=========================================="
echo "✅ Všechny simulace dokončeny!"
echo "=========================================="

