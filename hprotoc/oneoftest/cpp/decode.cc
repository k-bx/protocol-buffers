#include <fcntl.h>
#include <iostream>
#include <fstream>
#include <streambuf>
#include <string>

#include <google/protobuf/text_format.h>
#include <google/protobuf/io/zero_copy_stream_impl.h>
#include "school.pb.h"

using namespace std;

int main(int argc, char* argv[])
{
  GOOGLE_PROTOBUF_VERIFY_VERSION;

  if( argc != 2 ) {
    cerr << "Usage: " << argv[0] << " File" << endl;
    exit(-1);
  }
  char* filename = argv[1];
  
  dormitory d;

  fstream input(filename, ios::in | ios::binary);
  d.ParseFromIstream(&input);

  string str;   
  google::protobuf::TextFormat::PrintToString( d , &str);
  cout << str << endl;

  fstream output( argv[2], ios::out | ios::trunc | ios::binary );
  d.SerializeToOstream(&output);
  
}
