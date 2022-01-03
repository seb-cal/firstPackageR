#' French holidays dataset
#'
#' A dataset containing the dates of the 11 main french holidays
#' from 2005 to 2016
#'
#' @format A data frame with 132 rows and 2 variables:
#' \describe{
#'   \item{ds}{date of holiday}
#'   \item{holiday}{Name of holiday}
#' }
#' @source \url{https://www.kaggle.com/ahmedlahlou/accidents-in-france-from-2005
#' -to-2016}
"holidays"


#' Accident details
#'
#' Details on the circumstances of accidents in France between 2005 and 2016
#'
#'
#' @format A data frame with 839985 rows and 31 variables:
#' \describe{
#'  \item{Num_Acc}{Accident ID}
#'   \item{hrmn}{Time of accident}
#'   \item{lum}{Lighting conditions in which the accident occurred
#'    \itemize{
#'     \item{1 : }{Full day}
#'     \item{2 : }{Twilight or dawn}
#'     \item{3 : }{Night without public lighting}
#'     \item{4 : }{Night with public lighting not lit}
#'     \item{5 : }{Night with public lighting on}
#'    }
#'   }
#'   \item{dep}{Department : INSEE Code (National Institute of Statistics and
#'   Economic Studies) of the departmeent followed by a 0 (201 Corse-du-Sud -
#'   202 Haute-Corse)}
#'   \item{com}{Municipality: The commune number is a code given by INSEE. The
#'   code has 3 numbers set to the right.}
#'   \item{agg}{Localisation : Surounding building density
#'    \itemize{
#'     \item{1 : Out of agglomeration}
#'     \item{2 : In built-up areas}
#'    }
#'   }
#'   \item{int}{Type of Intersection
#'    \itemize{
#'     \item{1 : Out of intersection}
#'     \item{2 : Intersection in X}
#'     \item{3 : Intersection in T}
#'     \item{4 : Intersection in Y}
#'     \item{5 : Intersection with more than 4 branches}
#'     \item{6 : Giratory}
#'     \item{7 : Place}
#'     \item{8 : Level crossing}
#'     \item{9 : Other intersection}
#'    }
#'   }
#'   \item{atm}{Atmospheric conditions
#'    \itemize{
#'     \item{1 : Normal}
#'     \item{2 : Light rain}
#'     \item{3 : Heavy rain}
#'     \item{4 : Snow - hail}
#'     \item{5 : Fog / smoke}
#'     \item{6 : Strong wind / storm}
#'     \item{7 : Dazzling weather}
#'     \item{8 : Cloudy weather}
#'     \item{9 : Other}
#'    }
#'   }
#'   \item{col}{Type of collision
#'    \itemize{
#'     \item{1 : Two vehicles - frontal}
#'     \item{2 : Two vehicles - from the rear}
#'     \item{3 : Two vehicles - by the side}
#'     \item{4 : Three vehicles and more - in chain}
#'     \item{5 : Three or more vehicles - multiple collisions}
#'     \item{6 : Other collision}
#'     \item{7 : Without collision}
#'    }
#'   }
#'   \item{adr}{Postal address: variable filled in for accidents occurring in
#'   built-up areas}
#'   \item{gps}{GPS coding: 1 originator character:
#'    \itemize{
#'     \item{M = Metropole}
#'     \item{A = Antilles (Martinique or Guadeloupe)}
#'     \item{G = Guyane}
#'     \item{R = Reunion}
#'     \item{Y = Mayotte}
#'    }
#'   }
#'   \item{lat}{Geographic latitude}
#'   \item{long}{Geographic longitude}
#'  \item{catr}{Category of road:
#'   \itemize{
#'    \item{1 : Highway}
#'    \item{2 : National Road}
#'    \item{3 : Departmental Road}
#'    \item{4 : Communal Road}
#'    \item{5 : Off public network}
#'    \item{6 : Parking lot open to public traffic}
#'    \item{9 : other}
#'   }
#'  }
#'  \item{voie}{Road Number}
#'  \item{V1}{Numeric index of the route number (example: 2 bis, 3 ter etc.)}
#'  \item{V2}{Letter alphanumeric index of the road}
#'  \item{circ}{Traffic regime:
#'   \itemize{
#'    \item{1 : One way}
#'    \item{2 : Bidirectional}
#'    \item{3 : Separated carriageways}
#'    \item{4 :  With variable assignment channels}
#'   }
#'  }
#'  \item{nbv}{Total number of traffic lanes}
#'  \item{vosp}{Indicates the existence of a reserved lane, regardless of whether or not the accident occurs on that lane.
#'   \itemize{
#'    \item{1 : Bike path}
#'    \item{2 : ycle Bank}
#'    \item{3 : Reserved channel}
#'   }
#'  }
#'  \item{Prof}{Longitudinal profile describes the gradient of the road at the accident site
#'   \itemize{
#'    \item{1 : Dish}
#'    \item{2 : Slope}
#'    \item{3 : Hilltop}
#'    \item{4 : Hill bottom}
#'   }
#'  }
#'  \item{pr}{Home PR number (upstream terminal number)}
#'  \item{pr1}{Distance in meters to the PR (relative to the upstream terminal)}
#'  \item{plan}{Drawing in plan:
#'   \itemize{
#'    \item{1 :  Straight part}
#'    \item{2 : Curved on the left}
#'    \item{3 : Curved right}
#'    \item{4 : In "S"}
#'    }
#'  }
#'  \item{lartpc}{Central solid land width (TPC) if there is}
#'  \item{larrout}{Width of the roadway assigned to vehicle traffic are not included the emergency stop strips, CPRs and parking spaces}
#'  \item{surf}{surface condition
#'   \itemize{
#'    \item{1 : normal}
#'    \item{2 : wet}
#'    \item{3 : puddles}
#'    \item{4 : flooded}
#'    \item{5 : snow}
#'    \item{6 : mud}
#'    \item{7 : icy}
#'    \item{8 : fat - oil}
#'    \item{9 : other}
#'   }
#'  }
#'  \item{infra}{Development - Infrastructure:
#'   \itemize{
#'    \item{1 : Underground - tunnel}
#'    \item{2 : Bridge - autopont}
#'    \item{3 : Exchanger or connection brace}
#'    \item{4 : Railway}
#'    \item{5 : Carrefour arranged}
#'    \item{6 : Pedestrian area}
#'    \item{7 : Toll zone}
#'   }}
#'  \item{situ}{Situation of the accident:
#'   \itemize{
#'    \item{1 : On the road}
#'    \item{2 : On emergency stop band}
#'    \item{3 : On the verge}
#'    \item{4 : On the sidewalk}
#'    \item{5 : On bike path}
#'    }
#'  }
#'  \item{env1}{school point: near a school}
#' }
#' @source \url{https://www.kaggle.com/ahmedlahlou/accidents-in-france-from-2005
#' -to-2016}
"acc"


#' Users in accident
#'
#' A dataset with details about the users implicated in the accidents logged in
#' the dataset caracteristics and places
#'
#' @format A data frame with 1876005 rows and 12 variables:
#' \describe{
#'   \item{Acc_number}{Accident identifier}
#'   \item{Num_Veh}{Identification of the vehicle taken back for each user
#'   occupying this vehicle (including pedestrians who are attached to the
#'   vehicles that hit them)}
#'   \item{place}{Allows to locate the place occupied in the vehicle by the
#'    user at the time of the accident}
#'   \item{catu}{User category:
#'    \itemize{
#'     \item{1 : Driver}
#'     \item{2 : Passenger}
#'     \item{3 : Pedestrian}
#'     \item{4 : Pedestrian in rollerblade or scooter}
#'    }
#'   }
#'   \item{grav}{everity of the accident: The injured users are classified into
#'   three categories of victims plus the uninjured
#'    \itemize{
#'     \item{Unscathed}
#'     \item{Killed}
#'     \item{Hospitalized wounded}
#'     \item{Light injury}
#'    }
#'   }
#'   \item{sex}{Sex of the user
#'    \itemize{
#'     \item{Male}
#'     \item{Female}
#'    }
#'   }
#'   \item{Year_on}{Year of birth of the user}
#'   \item{trip}{Reason for traveling at the time of the accident:
#'    \itemize{
#'     \item{1 : Home - work}
#'     \item{2 : Home - school}
#'     \item{3 : Shopping - Shopping}
#'     \item{4 : Professional use}
#'     \item{5 : Promenade - leisure}
#'     \item{9 : Other}
#'    }
#'   }
#'   \item{secu}{on 2 characters:
#'    \itemize{the first concerns the existence of a safety equipment
#'     \item{1 : Belt}
#'     \item{2 : Helmet}
#'     \item{3 : Children's device}
#'     \item{4 : Reflective equipment}
#'     \item{9 : Other}
#'    }
#'    \itemize{the second is the use of Safety Equipment
#'     \item{1 : Yes}
#'     \item{2 : No}
#'     \item{3 :  Not determinable}
#'     }
#'   }
#'   \item{locp}{Location of the pedestrian:
#'    \itemize{On pavement:
#'     \item{1 : More than 50 m from the pedestrian crossing}
#'     \item{2 : Less than 50 m from the pedestrian crossing}
#'    }
#'    \itemize{On pedestrian crossing:
#'     \item{3 : Without light signaling}
#'     \item{4 : With light signaling}
#'    }
#'    \itemize{Various:
#'     \item{5 : On the sidewalk}
#'     \item{6 : On the verge}
#'     \item{7 : On refuge or BAU}
#'     \item{8 : On against aisle}
#'    }
#'   }
#'   \item{actp}{Action of the pedestrian:
#'    \itemize{Moving
#'     \item{0 : not specified or not applicable}
#'     \item{1 : Meaning bumping vehicle}
#'     \item{2 : Opposite direction of the vehicle}
#'     }
#'    \itemize{Various
#'     \item{3 : Crossing}
#'     \item{4 : Masked}
#'     \item{5 : Playing - running}
#'     \item{6 : With animal}
#'     \item{9 : Other}
#'     }
#'   }
#'   \item{etatp}{This variable is used to specify whether the injured
#'   pedestrian was alone or not
#'    \itemize{
#'     \item{1 : Only}
#'     \item{2 : Accompanied}
#'     \item{3 : In a group}
#'    }
#'   }
#'
#' }
#' @source \url{https://www.kaggle.com/ahmedlahlou/accidents-in-france-from-2005
#' -to-2016}
"users"



#' Vehicles in accident
#'
#' A dataset with details about the vehicles implicated in the accidents
#' logged in the dataset caracteristics and places
#'
#' @format A data frame with 1433389 rows and 9 variables:
#' \describe{
#'   \item{Num_Acc}{Accident ID}
#'   \item{Num_Veh}{Identification of the vehicle taken back for each user
#'   occupying this vehicle (including pedestrians who are attached to vehicles
#'   that hit them) - alphanumeric code}
#'   \item{GP}{
#'    \itemize{
#'     \item{1 : PK or PR or increasing postal address number}
#'     \item{2 : PK or PR or descending postal address number}
#'    }}
#'   \item{CATV}{Category of vehicle:
#'    \itemize{
#'     \item{Bicycle}
#'     \item{Moped <50cm3}
#'     \item{Cart (Quadricycle with bodied motor) (formerly "cart or motor
#'     tricycle")}
#'     \item{Not used since 2006 (registered scooter)}
#'     \item{Not used since 2006 (motorcycle)}
#'     \item{Not used since 2006 (side-car)}
#'     \item{VL only}
#'     \item{Not used category (VL + caravan)}
#'     \item{Not used category (VL + trailer)}
#'     \item{VU only 1,5T <= GVW <= 3,5T with or without trailer (formerly VU
#'     only 1,5T <= GVW <= 3,5T)}
#'     \item{Most used since 2006 (VU (10) + caravan)}
#'     \item{Most used since 2006 (VU (10) + trailer)}
#'     \item{PL only 3,5T}
#'    }
#'   }
#' }
#' @source \url{https://www.kaggle.com/ahmedlahlou/accidents-in-france-from-2005
#' -to-2016}
"vehicles"


