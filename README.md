## Installation Serialbox

- Clone repo

  ```bash
  git clone git@github.com:GridTools/serialbox.git
  cd serialbox
  mkdir build && cd build
  ```
- Build

  ```bash
  cmake .. -DSERIALBOX_ENABLE_FORTRAN=ON 
  cmake --build . --target install
  ```

## Utilisation du préprocesseur Serialbox

- Variables d'environnement

```bash
export SERIALBOX_ROOT=$HOME/.local
```

- Exécution du préprocesseur

```bash
$SERIALBOX_ROOT/python/pp_ser/pp_ser.py test.F90 --output-dir=./generated
```

- Préprocesseur dans CMake

```bash

```
