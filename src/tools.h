#ifndef TOOLS_H
#define TOOLS_H
#include <string>
#include <vector>
size_t split(const std::string& txt, std::vector<std::string>& strs, char ch);

void replace_all(std::string& str, std::string to_replace, std::string replaced_val );
#endif //! TOOLS_H