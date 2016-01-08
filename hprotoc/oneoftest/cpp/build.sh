protoc --proto_path=.. --cpp_out=. ../sample.proto
g++ -c main.cc `pkg-config --cflags protobuf`
g++ -c sample.pb.cc `pkg-config --cflags protobuf`
g++ -o main main.o sample.pb.o `pkg-config --libs protobuf`
