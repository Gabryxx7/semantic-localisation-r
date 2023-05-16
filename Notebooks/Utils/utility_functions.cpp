#include <Rcpp.h>
using namespace Rcpp;

// dtCol should be a column of type list, each cell should be a character vector e.g c("1", "2")
// [[Rcpp::export]]
List column_vector_append(List dtCol, CharacterVector roomsVisited, String currentRoom, String agentId) {
  int length =  dtCol.size();
  if(length != roomsVisited.size()){
    std::ostringstream sstream;
    sstream << "Mismatching size: The passed columns has length " << length << " while roomsVisited has length : " << roomsVisited.size();
    stop(sstream.str());
    return List();
  }
  for(int i=0; i < length; i++){
    String room = roomsVisited[i];
    if(room == currentRoom){
      if(dtCol[i] == R_NilValue){
        // Rprintf("dotCol[%i] is NULL.\n\n", i+1);
        dtCol[i] = CharacterVector::create(agentId);
        continue;
      }
      CharacterVector cellData = dtCol[i];
      cellData.push_back(agentId);
      dtCol[i] = cellData;
    }
  }
  return dtCol;
}

// [[Rcpp::export]]
void type_print(List dtCol) {
  Rcout << "Length of column: " << dtCol.size();
  CharacterVector firstEl = dtCol[1];
  Rcout << "\nLength of cell " << firstEl.size() << ":\t" << firstEl;
}

// [[Rcpp::export]]
NumericVector customRLE(NumericVector timeSlotID, NumericVector diff) {
  int n = timeSlotID.size();
  int groupId = 0;
  NumericVector groups(n);
  groups[0] = groupId;
  for(int i = 1; i < n; ++i) {
    if(diff[i] > 1){
      groups[i] = ++groupId;
    }
    else{
      groups[i] = groupId;
    }
  }
  return groups;
}
