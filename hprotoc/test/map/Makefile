test: hs/encode_map hs/decode_map cpp/encode_map cpp/decode_map
	echo "\nHS -> HS\n" && \
	./hs/encode_map | ./hs/decode_map && \
	echo "\nHS -> CPP\n" && \
	./hs/encode_map | ./cpp/decode_map && \
	echo "\nCPP -> HS\n" && \
	./cpp/encode_map | ./hs/decode_map

hs/encode_map: hs/encode_map.hs
	cd hs && make encode_map

hs/decode_map: hs/decode_map.hs
	cd hs && make decode_map

cpp/encode_map: cpp/encode_map.cc
	cd cpp && make encode_map

cpp/decode_map: cpp/decode_map.cc
	cd cpp && make decode_map

clean:
	cd hs; make clean; cd ..; cd cpp; make clean

.PHONY: clean
