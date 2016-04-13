GHC=stack exec ghc --

HPROTOC=stack exec hprotoc --

test: encode_map decode_map
	./encode_map | ./decode_map

decode_map: Mymap decode_map.hs
	${GHC} --make -O1 decode_map.hs

encode_map: Mymap SomeType encode_map.hs
	${GHC} --make -O1 encode_map.hs

Mymap:
	${HPROTOC} --proto_path=.. mymap.proto

SomeType:
	$(HPROTOC) --proto_path=.. some_type.proto

clean:
	rm -rf encode_map decode_map Mymap Mymap.hs *.{hi,o} Some_type Some_type.hs

.PHONY: clean test
