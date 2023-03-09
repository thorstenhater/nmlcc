cmake_minimum_required(VERSION 3.22)
project(arbor-nmlcc LANGUAGES CXX)
set(CMAKE_CXX_STANDARD 17)

find_package(arbor REQUIRED)
find_package(nlohmann_json 3.11.0 REQUIRED)

add_executable(main main.cxx)
target_link_libraries(main PUBLIC arbor::arbor arbor::arborio arbor::arborenv nlohmann_json::nlohmann_json)
