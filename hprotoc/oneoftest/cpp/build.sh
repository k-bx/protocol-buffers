protoc --proto_path=.. --cpp_out=. school.proto
g++ -c encode.cc `pkg-config --cflags protobuf`
g++ -c decode.cc `pkg-config --cflags protobuf`
g++ -c school.pb.cc `pkg-config --cflags protobuf`
g++ -o encode encode.o school.pb.o `pkg-config --libs protobuf`
g++ -o decode decode.o school.pb.o `pkg-config --libs protobuf`
