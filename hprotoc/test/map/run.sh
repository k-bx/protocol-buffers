#!/bin/bash
cd hs/ && make && cd ..
cd cpp/ && make && cd ..
./hs/encode_map && ./cpp/decode_map
rm mymap.output
cd cpp/ && make clean && cd ..
cd hs/ && make clean && cd ..
