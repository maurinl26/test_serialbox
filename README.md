## Installation Serialbox

- Clone repo

  ```bash
  git clone git@github.com:GridTools/serialbox.git
  cd serialbox
  mkdir build && cd build
  ```
- Build

  ```bash
  cmake .. -DCMAKE_INSTALL_PREFIX=$HOME/install/serialbox -DSERIALBOX_ENABLE_C=ON -DSERIALBOX_ENABLE_FORTRAN=ON
  make install -j8
  ```

## Compilation avec gfortran (sur Atos CEP)

```bash
cd ./generated
gfortran test.F90      $SERIALBOX_INSTALL_PATH/lib/libSerialboxFortran.a     $SERIALBOX_INSTALL_PATH/lib/libSerialboxC.a     $SERIALBOX_INSTALL_PATH/lib/libSerialboxCore.a     -lstdc++  -pthread   -DSERIALIZE     -I$HOME/test_serialbox/generated     -o test.bin
```


## Utilisation du préprocesseur Serialbox

- Variables d'environnement

```bash
export SERIALBOX_ROOT=$HOME/install/serialbox
```

- Exécution du préprocesseur

```bash
$SERIALBOX_ROOT/python/pp_ser/pp_ser.py test.F90 --output-dir=./generated
```

- Préprocesseur dans CMake

```bash

```
