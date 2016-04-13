test: hs/encode_map hs/decode_map
	@echo "\nHS -> HS\n" && \
	./hs/encode_map | ./hs/decode_map

hs/encode_map: hs/encode_map.hs
	cd hs && make encode_map

hs/decode_map: hs/decode_map.hs
	cd hs && make decode_map

clean:
	cd hs; make clean; cd ..;

.PHONY: clean
