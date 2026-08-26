#### GUI development settings

QT_PATH="/opt/homebrew/opt/qt/bin"

function qtmake {
    cmake -DCMAKE_PREFIX_PATH=$(brew --prefix qt)
}
