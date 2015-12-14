#include <fcntl.h>
#include <iostream>
#include <fstream>
#include <streambuf>
#include <string>

#include <google/protobuf/text_format.h>
#include <google/protobuf/io/zero_copy_stream_impl.h>
#include "sample.pb.h"

using namespace std;

int main1(int argc, char* argv[])
{
  //GOOGLE_PROTOBUF_VERIFY_VERSION;

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

  u1 = a.add_users();
  u1->set_id( 45 );
  u1->set_name( "world");
  
  // a.SerializeToString(&data);
  google::protobuf::TextFormat::PrintToString( a , &data );

  cout << data << endl;
   
  

  
}


int main(int argc, char* argv[])
{
  GOOGLE_PROTOBUF_VERIFY_VERSION;

  if( argc != 2 ) {
    cerr << "Usage: " << argv[0] << " File" << endl;
  }
  
  sample_message *a = new sample_message();
  ifstream input;
  input.open(argv[1]);

  int fileDescriptor = open(argv[1],O_RDONLY);
  google::protobuf::io::FileInputStream fileInput( fileDescriptor );
  // string data(istreambuf_iterator<char>(input), istreambuf_iterator< char>() );
  
  google::protobuf::TextFormat::Parse(&fileInput, a ) ;

  string text; 
  google::protobuf::TextFormat::PrintToString( *a , &text );

  cout << text << endl;
  
    
}
