protoc --proto_path=.. --cpp_out=. ../sample.proto
gcc -c main.cc `pkg-config --cflags protobuf`
gcc -c sample.pb.cc `pkg-config --cflags protobuf`
gcc -o main main.o sample.pb.o `pkg-config --libs protobuf`
