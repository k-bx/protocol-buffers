GHC=stack exec ghc --

HPROTOC=stack exec hprotoc --

test: encode_map decode_map
	./encode_map | ./decode_map

decode_map: Mymap decode_map.hs
	${GHC} --make -O1 decode_map.hs

encode_map: Mymap MapPackage encode_map.hs
	${GHC} --make -O1 encode_map.hs

Mymap:
	${HPROTOC} --proto_path=.. mymap.proto

MapPackage:
	$(HPROTOC) --proto_path=.. mapPackage.proto

clean:
	rm -rf encode_map decode_map Mymap Mymap.hs MapPackage MapPackage.hs *.{hi,o} tags

.PHONY: clean test
