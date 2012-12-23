// Copyright 2012 liquid_amber
//
// This file is part of PicasaDB.
//
// PicasaDB is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// PicasaDB is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with PicasaDB.  If not, see <http://www.gnu.org/licenses/>.

#include <iostream>
#include <fstream>
#include <string>
#include <cassert>
#include <cstdint>
#include <boost/format.hpp>

template <class T>
T pointer_cast(void * ptr)
{
  return static_cast<T>(static_cast<void *>(ptr));
}

template <class T>
const T pointer_cast(const void * ptr)
{
  return static_cast<T>(ptr);
}

template <class T>
void fill_vector(std::ifstream & ifs, std::vector<T> & vec)
{
  uint32_t length;
  ifs.read(pointer_cast<char*>(&length), sizeof(length));
  vec.resize(length);
  ifs.read(pointer_cast<char*>(&vec[0]), sizeof(T) * length);
}

inline void ensure_size(std::string & buf, size_t length)
{
  if(length > buf.size()) buf.resize(length);
}

void splitthumbs(const char * filename,
                 const char * directory,
                 const std::vector<uint32_t> & offset,
                 const std::vector<uint32_t> & size)
{
  std::ifstream ifs;
  ifs.open(filename, std::ios::in | std::ios::binary);
  if(!ifs) {
    std::cerr << "Failed to read image file: " << filename << "\n";
    exit(1);
  }
  assert(offset.size() == size.size());
  boost::format output_format("%s/%010u.jpg");
  std::string buf;
  for(size_t i=0; i < offset.size(); ++i) {
    std::string output_filename = (output_format % directory % i).str();
    std::ofstream ofs;
    ofs.open(output_filename.c_str(), std::ios::out | std::ios::binary | std::ios::trunc);
    if(size[i] != 0) {
      ifs.seekg(offset[i]);
      ensure_size(buf, size[i]);
      ifs.read(pointer_cast<char*>(&buf[0]), size[i]);
      ofs.write(pointer_cast<const char*>(&buf[0]), size[i]);
    }
    ofs.close();
  }
  ifs.close();
}

int main(const int argc, char ** argv)
{
  std::ifstream ifs;
  assert(argc > 3);
  const char * index = argv[1];
  const char * image = argv[2];
  const char * targetdir = argv[3];
  ifs.open(argv[1], std::ios::in | std::ios::binary);
  if(!ifs) {
    std::cerr << "Failed to read index file: " << index << "\n";
    exit(1);
  }
  //ensureHeader
  uint8_t buf[4];
  ifs.read(pointer_cast<char*>(buf), 4);
  assert(buf[0] == 0xCD);
  assert(buf[1] == 0xCC);
  assert(buf[2] == 0xCC);
  assert(buf[3] == 0x3F);
  ifs.read(pointer_cast<char*>(buf), 4);
  assert(buf[0] == 0x00);
  assert(buf[1] == 0x00);
  assert(buf[2] == 0x00);
  assert(buf[3] == 0x00);
  std::vector<uint32_t> info, offset, size;
  fill_vector(ifs, info);
  fill_vector(ifs, offset);
  fill_vector(ifs, size);
  ifs.close();
  splitthumbs(image, targetdir, offset, size);
  return 0;
}
