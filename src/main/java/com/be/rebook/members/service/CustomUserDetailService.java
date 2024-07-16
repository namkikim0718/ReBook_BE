package com.be.rebook.members.service;

import com.be.rebook.members.dto.CustomUserDetails;
import com.be.rebook.members.entity.Members;
import com.be.rebook.members.repository.MembersRepository;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.core.userdetails.UsernameNotFoundException;
import org.springframework.stereotype.Service;

@Service
public class CustomUserDetailService implements UserDetailsService {

    private final MembersRepository membersRepository;

    public CustomUserDetailService(MembersRepository membersRepository){
        this.membersRepository = membersRepository;
    }
    @Override
    public UserDetails loadUserByUsername(String username) throws UsernameNotFoundException {

        Members userData = membersRepository.findByUsername(username);
        System.out.println("loadUserByUsername "+ SecurityContextHolder.getContext().getAuthentication());
        if(userData != null){
            return new CustomUserDetails(userData);
        }

        return null;
    }
}
