#include <fcntl.h>
#include <iostream>
#include <fstream>
#include <streambuf>
#include <string>

#include <google/protobuf/text_format.h>
#include <google/protobuf/io/zero_copy_stream_impl.h>
#include "mymap.pb.h"

using namespace std;

int main()
{
  GOOGLE_PROTOBUF_VERIFY_VERSION;

  char filename[] = "mymap.output";

  WithMap msg;

  fstream input(filename, ios::in | ios::binary);
  msg.ParseFromIstream(&input);

  string str;
  google::protobuf::TextFormat::PrintToString(msg, &str);
  cout << str << endl;

  cout << "map_field_size: "
       << msg.map_field_size()
       << endl;

  for(auto& kv: msg.map_field())
      cout << "key="
           << kv.first
           << "\t"
           << "value="
           << kv.second.content()
           << endl;

  cout << endl;

  cout << "another_map_field_size: "
       << msg.another_map_field_size()
       << endl;

  for(auto& kv: msg.another_map_field())
      cout << "key="
           << kv.first
           << "\t"
           << "value="
           << kv.second
           << endl;
}
