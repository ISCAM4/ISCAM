#' Cloud Seeding Data
#'
#' Our lives depend on rainfall. Consequently, scientists have long investigated whether humans can
#' intervene and, as needed, help nature produce more rainfall.
#' In one study, researchers in southern Florida explored whether injecting silver
#' iodide into cumulus clouds would lead to increased rainfall.
#' On each of 52 days that were judged to be suitable for cloud seeding, a target cloud was identified and a
#' plane flew through the target cloud in order to seed it. Randomization was used to determine whether or
#' not to load a seeding mechanism and seed the target cloud with silver iodide on that day. Radar was
#' used to measure the volume of rainfall from the selected cloud during the next 24 hours. The results
#' from Simpson, Olsen, and Eden, (1975) measure rainfall in volume units of acre-feet, “height” of rain across
#' one acre.
#'
#' @format ## `CloudSeeding`
#' A data frame with 52 rows and 2 columns:
#' \describe{
#'   \item{treatment}{Whether a cloud was seeded with silver iodide or not.}
#'   \item{rainfall}{Volume of rainfall during the next 24 hours, in acre-feet.}
#' }
#' @keywords datasets
#' @source \doi{10.2307/1268346}
"CloudSeeding"

#' Flint Michigan Lead Data
#'
#' Lead poisoning can be a serious problem associated with drinking tap water. Many older water pipes are
#' made of lead. Over time, the pipes corrode, releasing lead into the drinking water. In April 2014, the city
#' of Flint Michigan switched its water supply to the Flint River in an effort to save money. The Michigan
#' Department of Environmental Quality (MDEQ) tested the water at the time and declared it safe to
#' drink. Officials were supposed to test at least 100 homes, targeting those most at risk. The U.S.
#' Environmental Protection Agency (EPA)’s Lead and Copper Rule states that if lead concentrations
#' exceed an action level of 15 parts per billion (ppb) in more than 10% of homes sampled, then actions
#' must be undertaken to control corrosion, and the public must be informed.
#'
#'
#' @format ## `flint`
#' A data frame with 71 rows and 1 column:
#' \describe{
#'   \item{lead}{Lead concentration per household, measured in parts per billion.}
#' }
#' @keywords datasets
"FlintMDEQ"

#' Infant Data
#'
#' In a study reported in the November 2007 issue of Nature, researchers investigated whether infants take
#' into account an individual’s actions towards others in evaluating that individual as appealing or aversive,
#' perhaps laying for the foundation for social interaction (Hamlin, Wynn, and Bloom, 2007). In other
#' words, do children who aren’t even yet talking still form impressions as to someone’s friendliness based
#' on their actions? In one component of the study, 10-month-old infants were shown a “climber”
#' character (a piece of wood with “googly” eyes glued onto it) that could not make it up a hill in two
#' tries. Then the infants were shown two scenarios for the climber’s next try, one where the climber was
#' pushed to the top of the hill by another character (the “helper” toy) and one where the climber was
#' pushed back down the hill by another character (the “hinderer” toy). The infant was alternately shown
#' these two scenarios several times. Then the child was presented with both pieces of wood (the helper
#' and the hinderer characters) and asked to pick one to play with. Videos demonstrating this component
#' of the study can be found at <https://campuspress.yale.edu/infantlab/media/>.
#'
#' @format ## `Infant`
#' A data frame with 16 rows and 1 column:
#' \describe{
#'   \item{choice}{Whether a baby selected the "helper" or "hinderer" toy.}
#' }
#' @keywords datasets
#' @source <https://pubmed.ncbi.nlm.nih.gov/18033298/>
"Infant"

#' Elephant Walking Data
#'
#' Researchers Holdgate et al. (2016) studied walking behavior of elephants in North American zoos to see
#' whether there is a difference in average distance traveled by African and Asian elephants in captivity.
#' They put GPS loggers on 33 African elephants and 23 Asian elephants, and measured the distance (in
#' kilometers) the elephants walked per day.
#'
#' @format ## `Elephants`
#' A data frame with 56 rows and 2 columns:
#' \describe{
#'   \item{Species}{What species this Elephant was.}
#'   \item{Distance}{How many kilometers they walked per day.}
#' }
#' @keywords datasets
#' @source \doi{10.1371/journal.pone.0150331}
"elephants"

#' Sleep Deprivation Data
#'
#' Researchers have established that sleep deprivation has a harmful effect on visual learning (the subject
#' does not consolidate information to improve on the task). Stickgold, James, and Hobson (2000)
#' investigated whether subjects could “make up” for sleep deprivation by getting a full night’s sleep in
#' subsequent nights. This study involved randomly assigning 21 subjects (volunteers between the ages of
#' 18 and 25) to one of two groups: one group was deprived of sleep on the night following training with a
#' visual discrimination task, and the other group was permitted unrestricted sleep on that first night. Both
#' groups were allowed unrestricted sleep on the following two nights, and then were re-tested on the third
#' day. Subjects’ performance on the test was recorded as the minimum time (in milliseconds) between
#' stimuli appearing on a computer screen for which they could accurately report what they had seen on the
#' screen. Previous studies had shown that subjects deprived of sleep performed significantly worse the
#' following day, but it was not clear how long these negative effects would last. The data presented here
#' are the improvements in reaction times (in milliseconds), so a negative value indicates a decrease in
#' performance.
#'
#' @format ## `SleepDeprivation`
#' A data frame with 21 rows and 2 columns:
#' \describe{
#'   \item{sleepcondition}{The sleep condition the subject was in.}
#'   \item{improvement}{The subject's improvement in reaction times, measured in milliseconds.}
#' }
#' @keywords datasets
#' @source <https://www.nature.com/articles/nn1200_1237>
"SleepDeprivation"
