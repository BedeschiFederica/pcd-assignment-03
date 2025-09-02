package main

import (
	"fmt"
	"math/rand"
	"time"
)

//type Ball struct{ move Move }

type Move int

const (
	Paper Move = iota
	Scissors
	Rock
)

var moveName = map[Move]string{
	Paper:    "paper",
	Scissors: "scissors",
	Rock:     "rock",
}

var result = map[[2]Move]string{
	{Rock, Scissors}:     "win",
	{Scissors, Paper}:    "win",
	{Paper, Rock}:        "win",
	{Scissors, Rock}:     "lose",
	{Paper, Scissors}:    "lose",
	{Rock, Paper}:        "lose",
	{Rock, Rock}:         "draw",
	{Scissors, Scissors}: "draw",
	{Paper, Paper}:       "draw",
}

func main() {
	moveChannel1 := make(chan Move)
	moveChannel2 := make(chan Move)
	resultChannel1 := make(chan string)
	resultChannel2 := make(chan string)
	go referee("referee", moveChannel1, moveChannel2, resultChannel1, resultChannel2)
	go player("p1", moveChannel1, resultChannel1)
	go player("p2", moveChannel2, resultChannel2)

	for {
	}
}

func referee(name string, moveChannel1 chan Move, moveChannel2 chan Move, resultChannel1 chan string,
	resultChannel2 chan string) {
	for {
		moveP1, moveP2 := <-moveChannel1, <-moveChannel2
		//fmt.Println(name, moveName[moveP1], moveName[moveP2])
		resultChannel1 <- result[[2]Move{moveP1, moveP2}]
		resultChannel2 <- result[[2]Move{moveP2, moveP1}]
		time.Sleep(1 * time.Second)
	}
}

func player(name string, moveChannel chan Move, resultChannel chan string) {
	for {
		move := Move(rand.Intn(3))
		moveChannel <- move
		result := <-resultChannel
		fmt.Println(name, moveName[move], result)
	}
}
