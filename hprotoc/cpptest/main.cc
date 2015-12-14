#include <iostream>
#include <string>

#include <google/protobuf/text_format.h>
#include "sample.pb.h"

using namespace std;

int main(int argc, char* argv[])
{
  GOOGLE_PROTOBUF_VERIFY_VERSION;

  sample_message a;
  sample_message_user* u1;
  
  string data; 
  
  a.set_key("key");
  a.set_value(30);
  a.set_name("hi");
  a.set_age(5);
  u1 = a.add_users( );
  u1->set_id( 30 );
  u1->set_name( "hello" );
  
  // a.SerializeToString(&data);
  google::protobuf::TextFormat::PrintToString( a , &data );

  cout << data << endl;
   
  

  
}
