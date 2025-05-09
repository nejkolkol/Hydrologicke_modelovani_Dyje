# Hydrologické modelování Dyje

Tento repozitář obsahuje sadu skriptů a dalších dokumentů použitých při práci s hydrologickým modelem povodí Dyje, který běží na platformě [mHM]([https://www.ufz.de/index.php?en=40145](https://mhm-ufz.org/)).

## Struktura repozitáře

- `namelists/`  
  Obsahuje hlavní namelisty modelu Dyje a soubory pro kalibraci.

- `shell_skripts/`  
  Shellové skripty pro hromadné spouštění simulací v mHM.

- `r_skripts/`  
  R skripty pro zpracování a vizualizaci výsledků.
  
  - `0p0078125_2024/`  
    Skripty zpracování simulace za rok 2024.
  
  - `0p0078125_scen/`  
    Skripty pro zpracování simulací pro klimatické scénáře.
    
    - `dalsi_skripty/`  
      Pomocné skripty pro shrnující výstupy.
  
  - `1975-2020/`  
    Skripty zpracující porovnání pozorování se simulacemi.

  - `kontrola_scen/`  
    Skripty kontrolující výsledky simulací pro klimatické scénáře.


