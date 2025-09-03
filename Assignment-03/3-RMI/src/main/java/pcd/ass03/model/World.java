package pcd.ass03.model;

import java.io.Serializable;
import java.rmi.Remote;
import java.rmi.RemoteException;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

public class World implements Serializable, Remote {
    private final int width;
    private final int height;
    private final List<Player> players;
    private final List<Food> foods;

    public World(int width, int height, List<Player> players, List<Food> foods) {
        this.width = width;
        this.height = height;
        this.players = List.copyOf(players); // Ensure immutability
        this.foods = List.copyOf(foods);     // Ensure immutability
    }

    public int getWidth() throws RemoteException {
        return width;
    }

    public int getHeight() throws RemoteException {
        return height;
    }

    public List<Player> getPlayers() throws RemoteException {
        return players;
    }

    public List<Food> getFoods() throws RemoteException {
        return foods;
    }

    public List<Player> getPlayersExcludingSelf(final Player player) throws RemoteException {
        return players.stream()
                .filter(p -> {
                    try {
                        return !p.getId().equals(player.getId());
                    } catch (RemoteException e) {
                        throw new RuntimeException(e);
                    }
                })
                .collect(Collectors.toList());
    }

    public Optional<Player> getPlayerById(final String id) throws RemoteException {
        return players.stream()
                .filter(p -> {
                    try {
                        return p.getId().equals(id);
                    } catch (RemoteException e) {
                        throw new RuntimeException(e);
                    }
                })
                .findFirst();
    }


    public World removePlayers(final List<Player> playersToRemove) throws RemoteException {
        List<String> idsToRemove = playersToRemove.stream().map(p -> {
            try {
                return p.getId();
            } catch (RemoteException e) {
                throw new RuntimeException(e);
            }
        }).toList();
        List<Player> newPlayers = players.stream()
                .filter(p -> {
                    try {
                        return !idsToRemove.contains(p.getId());
                    } catch (RemoteException e) {
                        throw new RuntimeException(e);
                    }
                })
                .collect(Collectors.toList());
        return new World(width, height, newPlayers, foods);
    }

    public World removeFoods(List<Food> foodsToRemove) throws RemoteException {
        List<Food> newFoods = foods.stream()
                .filter(f -> !foodsToRemove.contains(f)) // Assumes Food has proper equals/hashCode or relies on object identity if not overridden
                .collect(Collectors.toList());
        return new World(width, height, players, newFoods);
    }
}
