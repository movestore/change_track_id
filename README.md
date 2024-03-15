# Define Track IDs

MoveApps

Github repository: github.com/movestore/define_track_id

## Description
Tracks can be joined or split choosing any attribute present in the dataset.

## Documentation
The track ID is what defines each track. If you have downloaded the data from Movebank with the Movebank App, than by default each track will correspond to a deployment. One animal can have several deployments, and mostly it makes sense to separate them into different tracks, as there might be large gaps between these deployments. With this App you can:

(1) join several tracks into one, e.g. by choosing the individual_local_identifier, which will join all tracks of the same individual into one (keep in mind the possible gaps). Make sure you always join tracks in a way that data within a track only correspond to one individual.

(2) split one track into multiple tracks by using an attribute (also those created in another App) which has assigned to each location a season, behavioural state, etc (be aware that this could create an extreme large number of tracks).



### Input data
move2_loc

### Output data
move2_loc

### Artefacts
none

### Settings

`Attribute currently defining the tracks`: displays the names of the current track IDs. "n" displays the number of current tracks.

`Select attribute that will define the track Id`: displays the names of the selected track IDs. "n" displays the number of tracks that will be created.

`Store settings`: click to store the current settings of the app for future workflow runs

### Most common errors
The selected attribute needs to be a number or a character. Other formats like timestamps, or attributes that have units like e.g. weight, battery voltage, will give an error.

If the attribute selected joins tracks that correspond to several individuals, this will cause error in following Apps, or will produce misinforming results.

### Null or error handling


