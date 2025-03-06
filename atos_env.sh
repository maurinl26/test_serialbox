#!/bin/bash
module load cmake/3.28.3
module load gcc/9.3.0


# Serialbox installation
export CXX=$(which g++)
export CC=$(which gcc)
export FC=$(which gfortran)

#export BOOST_ROOT=/usr/include/boost
#export BOOST_INLCUDE=/usr/include/boost/include
#export LD_LIBRARY_PATH=${BOOST_ROOT}/lib:$LD_LIBRARY_PATH

export SERIALBOX_INSTALL_PATH=$HOME/install/serialbox
