GHC=stack exec ghc --

HPROTOC=stack exec hprotoc --

test: encode_map decode_map
	./encode_map | ./decode_map

decode_map: Mymap decode_map.hs
	${GHC} --make -O1 decode_map.hs

encode_map: Mymap encode_map.hs
	${GHC} --make -O1 encode_map.hs

Mymap:
	${HPROTOC} --proto_path=.. mymap.proto

clean:
	rm -rf decode_map encode_map Mymap Mymap.hs *.{hi,o}

.PHONY: clean test
