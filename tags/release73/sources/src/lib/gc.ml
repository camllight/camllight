#open "printf";;

let print_stat c =
  let st = stat () in
  fprintf c "minor_words: %d\n" st.minor_words;
  fprintf c "promoted_words: %d\n" st.promoted_words;
  fprintf c "major_words: %d\n" st.major_words;
  fprintf c "minor_collections: %d\n" st.minor_collections;
  fprintf c "major_collections: %d\n" st.major_collections;
  fprintf c "heap_words: %d\n" st.heap_words;
  fprintf c "heap_chunks: %d\n" st.heap_chunks;
  fprintf c "live_words: %d\n" st.live_words;
  fprintf c "live_blocks: %d\n" st.live_blocks;
  fprintf c "free_words: %d\n" st.free_words;
  fprintf c "free_blocks: %d\n" st.free_blocks;
  fprintf c "largest_words: %d\n" st.largest_words;
  fprintf c "fragments: %d\n" st.fragments;
;;
