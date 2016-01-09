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
  d.set_name("Gryffindor");
  member* m;
  m = d.add_members( );
  m->set_id( 1 );
  m->set_name( "Albus Dumbledore" );
  member_faculty* f;
  f = m->mutable_prop_faculty();
  f->set_subject("allmighty");
  f->set_title("headmaster");

  m = d.add_members();
  m->set_id( 2 );
  m->set_name( "Harry Potter");
  member_student *s;
  s = m->mutable_prop_student();
  s->set_grade(5);
  s->set_specialty("defense of dark arts");

  //string str;   
  //google::protobuf::TextFormat::PrintToString( d , &str);
  //cout << str << endl;

  fstream output( filename, ios::out | ios::trunc | ios::binary );
  d.SerializeToOstream(&output);
  
}

