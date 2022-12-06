"""
Day 6: Tuning Trouble
"""

PACKET_MARKER_LENGTH = 4


def read_signal():
    with open("puzzle-input.txt", "r") as puzzle_input:
        return puzzle_input.read().strip()


def main() -> None:
    # Part 1
    signal = read_signal()

    marker_found = False
    window_start, window_end = 0, PACKET_MARKER_LENGTH
    while not marker_found:
        marker_found = len(set(signal[window_start:window_end])) == PACKET_MARKER_LENGTH
        if not marker_found:
            window_start += 1
            window_end += 1

    print("Part 1:", window_end)


if __name__ == "__main__":
    main()
