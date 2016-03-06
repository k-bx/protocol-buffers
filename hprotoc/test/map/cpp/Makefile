decode_map: mymap.pb.o decode_map.o
	g++ -std=c++11 -o decode_map decode_map.o mymap.pb.o `pkg-config --libs protobuf`

mymap.pb.cc: ../mymap.proto
	protoc --proto_path=.. --cpp_out=. ../mymap.proto

decode_map.o: decode_map.cc
	g++ -std=c++11 -c decode_map.cc `pkg-config --cflags protobuf`

mymap.pb.o: mymap.pb.cc
	g++ -std=c++11 -c mymap.pb.cc `pkg-config --cflags protobuf`

clean:
	rm decode_map decode_map.o mymap.pb.{cc,h,o}

.PHONY: clean
