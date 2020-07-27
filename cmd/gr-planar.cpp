// https://www.boost.org/doc/libs/1_73_0/libs/graph/example/straight_line_drawing.cpp

#include <cstdio>
#include <cstdlib>
#include <iostream>
#include <vector>

#include <boost/graph/adjacency_list.hpp>
#include <boost/graph/boyer_myrvold_planar_test.hpp>
#include <boost/graph/chrobak_payne_drawing.hpp>
#include <boost/graph/graph_traits.hpp>
#include <boost/graph/is_straight_line_drawing.hpp>
#include <boost/graph/planar_canonical_ordering.hpp>
#include <boost/graph/properties.hpp>
#include <boost/property_map/property_map.hpp>

using namespace boost;

struct coord_t {std::size_t x; std::size_t y;};

typedef adjacency_list< vecS, vecS, undirectedS, property< vertex_index_t, int > > graph_t;

// 7 0 1 1 2 2 3 3 0 3 4 4 5 5 6 6 3 0 4 1 3 3 5 2 6 1 4 1 5 1 6
graph_t read_graph(void) {
    int i,j;
    if(scanf("%d",&i) != 1) { abort(); }
    graph_t g(i);
    while(scanf("%d %d",&i,&j) == 2) { add_edge(i,j,g); }
    return g;
}

int main(int argc, char** argv) {
    typedef std::vector< std::vector< graph_traits< graph_t >::edge_descriptor > > embedding_storage_t;
    typedef boost::iterator_property_map< embedding_storage_t::iterator, property_map< graph_t, vertex_index_t >::type > embedding_t;

    graph_t g = read_graph();

    embedding_storage_t embedding_storage(num_vertices(g));
    embedding_t embedding(embedding_storage.begin(), get(vertex_index, g));
    boyer_myrvold_planarity_test(boyer_myrvold_params::graph = g, boyer_myrvold_params::embedding = embedding);

    std::vector< graph_traits< graph_t >::vertex_descriptor > ordering;
    planar_canonical_ordering(g, embedding, std::back_inserter(ordering));

    typedef std::vector< coord_t > straight_line_drawing_storage_t;
    typedef boost::iterator_property_map< straight_line_drawing_storage_t::iterator, property_map< graph_t, vertex_index_t >::type > straight_line_drawing_t;
    straight_line_drawing_storage_t straight_line_drawing_storage(num_vertices(g));
    straight_line_drawing_t straight_line_drawing(straight_line_drawing_storage.begin(), get(vertex_index, g));

    chrobak_payne_straight_line_drawing(g, embedding, ordering.begin(), ordering.end(), straight_line_drawing);
    if (!is_straight_line_drawing(g, straight_line_drawing)) {
        abort();
    }

    graph_traits< graph_t >::vertex_iterator vi, vi_end;
    for (boost::tie(vi, vi_end) = vertices(g); vi != vi_end; ++vi) {
        coord_t coord(get(straight_line_drawing, *vi));
        std::cout << coord.x << " " << coord.y << std::endl;
    }

    return 0;
}
