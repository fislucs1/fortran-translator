module types
implicit none


integer, parameter :: dp=kind(0.d0), &          ! double precision
                      hp=selected_real_kind(15) ! high precision
                      
end module
