#include <Rcpp.h>
#include <fstream>
#include <sstream>
#include <iomanip>
#include <vector>
#include <cstdint>

using namespace Rcpp;

// Keccak-256 implementation
namespace keccak {
typedef std::uint64_t uint64_t;
typedef std::uint8_t uint8_t;

// Constants
const uint64_t keccakf_rndc[24] = {
  0x0000000000000001, 0x0000000000008082, 0x800000000000808a,
  0x8000000080008000, 0x000000000000808b, 0x0000000080000001,
  0x8000000080008081, 0x8000000000008009, 0x000000000000008a,
  0x0000000000000088, 0x0000000080008009, 0x000000008000000a,
  0x000000008000808b, 0x800000000000008b, 0x8000000000008089,
  0x8000000000008003, 0x8000000000008002, 0x8000000000000080,
  0x000000000000800a, 0x800000008000000a, 0x8000000080008081,
  0x8000000000008080, 0x0000000080000001, 0x8000000080008008
};

const int keccakf_rotc[24] = {
  1,  3,  6,  10, 15, 21, 28, 36, 45, 55, 2,  14,
  27, 41, 56, 8,  25, 43, 62, 18, 39, 61, 20, 44
};

const int keccakf_piln[24] = {
  10, 7,  11, 17, 18, 3, 5,  16, 8,  21, 24, 4,
  15, 23, 19, 13, 12, 2, 20, 14, 22, 9,  6,  1
};

// Bitwise rotation
inline uint64_t ROL64(uint64_t a, int offset) {
  return (a << offset) | (a >> (64 - offset));
}

// Keccak-f function
void keccakf(uint64_t st[25]) {
  uint64_t t, bc[5];

  for (int round = 0; round < 24; round++) {
    // Theta
    for (int i = 0; i < 5; i++) {
      bc[i] = st[i] ^ st[i + 5] ^ st[i + 10] ^ st[i + 15] ^ st[i + 20];
    }

    for (int i = 0; i < 5; i++) {
      t = bc[(i + 4) % 5] ^ ROL64(bc[(i + 1) % 5], 1);
      for (int j = 0; j < 25; j += 5) {
        st[j + i] ^= t;
      }
    }

    // Rho Pi
    t = st[1];
    for (int i = 0; i < 24; i++) {
      int j = keccakf_piln[i];
      bc[0] = st[j];
      st[j] = ROL64(t, keccakf_rotc[i]);
      t = bc[0];
    }

    // Chi
    for (int j = 0; j < 25; j += 5) {
      for (int i = 0; i < 5; i++) {
        bc[i] = st[j + i];
      }
      for (int i = 0; i < 5; i++) {
        st[j + i] ^= (~bc[(i + 1) % 5]) & bc[(i + 2) % 5];
      }
    }

    // Iota
    st[0] ^= keccakf_rndc[round];
  }
}

// Main keccak function
std::vector<uint8_t> keccak256(const uint8_t* in, size_t inlen) {
  uint64_t st[25] = {0};
  uint8_t temp[144] = {0};
  size_t rsiz = 136; // 1088 bits / 8
  size_t rsizw = rsiz / 8;

  // Absorb input
  for (size_t i = 0; i < inlen; i++) {
    temp[i % rsiz] ^= in[i];
    if ((i % rsiz) == (rsiz - 1)) {
      for (size_t j = 0; j < rsizw; j++) {
        st[j] ^= ((uint64_t*)temp)[j];
      }
      keccakf(st);
      memset(temp, 0, rsiz);
    }
  }

  // Padding
  temp[inlen % rsiz] ^= 0x01;
  temp[(rsiz - 1)] ^= 0x80;

  for (size_t j = 0; j < rsizw; j++) {
    st[j] ^= ((uint64_t*)temp)[j];
  }

  keccakf(st);

  // Output
  std::vector<uint8_t> md(32);
  for (size_t i = 0; i < 32; i++) {
    md[i] = ((uint8_t*)st)[i];
  }

  return md;
}
}

// Convert bytes to hex string
std::string bytes_to_hex(const std::vector<uint8_t>& bytes) {
  std::stringstream ss;
  ss << std::hex << std::setfill('0');
  for (const auto& byte : bytes) {
    ss << std::setw(2) << static_cast<int>(byte);
  }
  return ss.str();
}

// [[Rcpp::export]]
std::string keccak256_string(std::string input) {
  const uint8_t* data = reinterpret_cast<const uint8_t*>(input.c_str());
  std::vector<uint8_t> hash = keccak::keccak256(data, input.length());
  return bytes_to_hex(hash);
}

// [[Rcpp::export]]
std::string keccak256_raw(RawVector input) {
  const uint8_t* data = reinterpret_cast<const uint8_t*>(&input[0]);
  std::vector<uint8_t> hash = keccak::keccak256(data, input.size());
  return bytes_to_hex(hash);
}

// [[Rcpp::export]]
std::string keccak256_file(std::string filepath) {
  std::ifstream file(filepath, std::ios::binary);
  if (!file) {
    stop("Could not open file: " + filepath);
  }

  // Read file in chunks
  const size_t BUFFER_SIZE = 8192;
  std::vector<uint8_t> buffer(BUFFER_SIZE);
  std::vector<uint8_t> content;

  while (file) {
    file.read(reinterpret_cast<char*>(buffer.data()), BUFFER_SIZE);
    std::streamsize count = file.gcount();
    if (count == 0) break;
    content.insert(content.end(), buffer.begin(), buffer.begin() + count);
  }

  std::vector<uint8_t> hash = keccak::keccak256(content.data(), content.size());
  return bytes_to_hex(hash);
}
