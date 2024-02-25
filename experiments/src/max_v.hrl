
-record(metric, {
    density :: density:density(),
    top_io :: max_v:y(),
    top_lab :: max_v:y(),
    left_io :: max_v:x(),
    left_lab :: max_v:x(),
    right_io :: max_v:x(),
    right_lab :: max_v:x(),
    bottom_io :: max_v:y(),
    bottom_lab :: max_v:y(),
    indent_left_io :: max_v:x(),
    indent_left_lab :: max_v:x(),
    indent_right_lab :: max_v:x(),
    indent_bottom_io :: max_v:y(),
    indent_bottom_lab :: max_v:y(),
    pattern_x :: 0..3,
    pattern_y :: 0..3
}).

