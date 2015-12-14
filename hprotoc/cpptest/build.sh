protoc --proto_path=.. --cpp_out=. ../sample.proto
gcc -c main.cc
gcc -c sample.pb.cc
gcc -o main main.o sample.pb.o `pkg-config --libs protobuf`
