"""
Day 6: Tuning Trouble
"""

END_OF_PACKET_MARKER_LENGTH = 4
START_OF_PACKET_MARKER_LENGTH = 14


def read_signal():
    with open("puzzle-input.txt", "r") as puzzle_input:
        return puzzle_input.read().strip()


def num_chars_before_end_of_first_marker(marker_lengh: int) -> int:
    signal = read_signal()

    window_start, window_end = 0, marker_lengh
    while not len(set(signal[window_start:window_end])) == marker_lengh:
        window_start += 1
        window_end += 1

    return window_end


def main() -> None:
    # Part 1
    print("Part 1:", num_chars_before_end_of_first_marker(END_OF_PACKET_MARKER_LENGTH))

    # Part 2
    print("Part 2:", num_chars_before_end_of_first_marker(START_OF_PACKET_MARKER_LENGTH))


if __name__ == "__main__":
    main()
