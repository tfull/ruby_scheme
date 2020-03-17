FROM rust:latest

RUN useradd -m rust

COPY ./Cargo.toml /home/rust/Cargo.toml
COPY ./src /home/rust/src

RUN chown -R rust:rust /home/rust/
