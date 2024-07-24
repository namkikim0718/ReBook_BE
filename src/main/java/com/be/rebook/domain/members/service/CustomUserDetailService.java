package com.be.rebook.domain.members.service;

import com.be.rebook.domain.members.dto.CustomUserDetails;
import com.be.rebook.domain.members.entity.Members;
import com.be.rebook.domain.members.repository.MembersRepository;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.core.userdetails.UsernameNotFoundException;
import org.springframework.stereotype.Service;

@Service
public class CustomUserDetailService implements UserDetailsService {

    private final MembersRepository membersRepository;

    private static final Logger customUserDetailServiceLogger = LoggerFactory.getLogger(CustomUserDetailService.class);

    public CustomUserDetailService(MembersRepository membersRepository){
        this.membersRepository = membersRepository;
    }
    @Override
    public UserDetails loadUserByUsername(String username) throws UsernameNotFoundException {

        Members userData = membersRepository.findByUsername(username);
        Authentication curAuth = SecurityContextHolder.getContext().getAuthentication();
        customUserDetailServiceLogger.info("loadUserByUsername: {}", curAuth);
        if(userData != null){
            return new CustomUserDetails(userData);
        }

        return null;
    }
}
