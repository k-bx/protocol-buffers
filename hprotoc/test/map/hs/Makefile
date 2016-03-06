encode_map: Mymap
	stack exec ghc -- --make -O1 encode_map.hs

Mymap:
	stack exec hprotoc -- --proto_path=.. mymap.proto

clean:
	rm -rf encode_map Mymap Mymap.hs *.{hi,o}

.PHONY: clean
