#include <fcntl.h>
#include <iostream>
#include <fstream>
#include <streambuf>
#include <string>
#include <map>

#include <google/protobuf/text_format.h>
#include <google/protobuf/io/zero_copy_stream_impl.h>
#include "mymap.pb.h"

using namespace std;

int main()
{
  GOOGLE_PROTOBUF_VERIFY_VERSION;

  WithMap msg;
  auto* maf = msg.mutable_another_map_field();
  for(auto& x: {1,2,3,4})
      maf->insert({x,x});
  auto* mf = msg.mutable_map_field();
  auto vt = value_type();
  vt.set_content("World!");
  mf->insert({"Hello", vt});
  msg.SerializeToOstream(&std::cout);
}

